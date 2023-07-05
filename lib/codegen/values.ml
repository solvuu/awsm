open! Core
open! Import

let ec2_error_module () =
  let loc = !Ast_helper.default_loc in
  let all_errors = Ec2_errors.enumerate_all () in
  let string_to_code =
    let case name =
      Ast_helper.Exp.case
        (Ast_convenience.pstr name)
        (Ast_convenience.variant (Shape.capitalized_id name) None)
    in
    (all_errors |> List.map ~f:(fun { name; description = _ } -> name) |> List.map ~f:case)
    @ [ Ast_helper.Exp.case (Ast_convenience.pvar "name") [%expr `Unknown_code name] ]
    |> Ast_helper.Exp.match_ [%expr name]
  in
  let gen_variants enumeration =
    let case name typ =
      { prf_desc = Rtag ({ txt = name; loc = Location.none }, false, typ)
      ; prf_loc = Location.none
      ; prf_attributes = []
      }
    in
    let cases =
      enumeration
      |> List.map ~f:(fun { Ec2_errors.name; description = _ } -> name)
      |> List.map ~f:(fun name -> case (Shape.capitalized_id name) [])
    in
    let catch_all_error_case = case "Unknown_code" [ [%type: string] ] in
    Ast_helper.Typ.mk (Ptyp_variant (cases @ [ catch_all_error_case ], Closed, None))
  in
  let common_client_errors =
    gen_variants (Ec2_errors.Common_client_errors.enumerate ())
  in
  let client_errors_for_specific_actions =
    gen_variants (Ec2_errors.Client_errors_for_specific_actions.enumerate ())
  in
  let server_errors = gen_variants (Ec2_errors.Server_errors.enumerate ()) in
  [%str
    module Ec2_error = struct
      type common_client_errors = [%t common_client_errors] [@@deriving sexp]

      type client_errors_for_specific_actions = [%t client_errors_for_specific_actions]
      [@@deriving sexp]

      type server_errors = [%t server_errors] [@@deriving sexp]

      type code =
        [ common_client_errors
        | client_errors_for_specific_actions
        | server_errors
        ]
      [@@deriving sexp]

      type t = code * string option [@@deriving sexp]

      let string_to_code name = [%e string_to_code]

      let of_xml = function
        | `Data _ as xml ->
          failwithf
            "Ec2_error.of_xml: expected element, got data: %s"
            (Awsm.Xml.to_string xml)
            ()
        | `El (((_, name), _), _) as xml -> (
          match name with
          | "Response" -> (
            let data = function
              | `Data s -> s
              | `El (_, children) ->
                List.map children ~f:(function
                  | `Data s -> s
                  | `El _ -> "")
                |> Core.String.concat ~sep:""
            in
            let _request_id = data (Awsm.Xml.child_exn xml "RequestID") in
            let errors =
              Awsm.Xml.child_exn xml "Errors"
              |> Awsm.Xml.all_children
              |> List.map ~f:(fun error ->
                   let code = Awsm.Xml.child_exn error "Code" |> data in
                   let message = Awsm.Xml.child_exn error "Message" |> data in
                   string_to_code code, Some message)
            in
            match errors with
            | [] -> failwithf "Ec2_error.of_xml: no errors in Ec2 error response" ()
            | [ error ] -> error
            | _lst -> failwithf "Ec2_error.of_xml: multiple errors not supported" ())
          | name ->
            failwithf
              "Ec2_error: expected 'Response' tag got '%s': %s"
              name
              (Awsm.Xml.to_string xml)
              ())
      ;;
    end]
;;

let preamble ~loc () =
  [%str
    open! Core
    open Awsm
    open! Import]
;;

let errors (service : Botodata.service) =
  match service.metadata.protocol with
  | `json | `rest_json | `rest_xml | `query -> []
  | `ec2 -> ec2_error_module ()
;;

let constants ~awsm_service_id service =
  Service_structure.constants_of_service ~awsm_service_id service
;;

let constructors ~loc =
  [%str
    let simple_to_json to_value x = Botodata.Json.value_to_json_scalar (to_value x)
    let composed_to_json to_value x = Botodata.Json.value_to_json (to_value x)
    let to_query to_value x = Client.Query.of_value (to_value x)

    let structure_to_value_aux st ~f =
      let filter = function
        | k, Some v -> Some (k, v)
        | _ -> None
      in
      let pair k v = k, v in
      let defer_value (k, dv) = pair k dv in
      List.filter_map st ~f:filter |> List.map ~f:defer_value |> fun x -> `Structure (f x)
    ;;

    let structure_to_value = structure_to_value_aux ~f:Fn.id

    let structure_to_wrapped_value ~wrapper ~response =
      structure_to_value_aux ~f:(fun x ->
        [ wrapper, `Structure x; response, `Structure [] ])
    ;;]
;;

let mod_of_str s = s |> Ast_convenience.lid |> Ast_helper.Mod.ident

let open_all ~loc =
  List.map ~f:(fun s -> [%stri open [%m mod_of_str s] [@@warning "-33"]])
;;

let include_all ~loc = List.map ~f:(fun s -> [%stri include [%m mod_of_str s]])

let structure_singleton ~awsm_service_id ~loc service shape_modules =
  preamble ~loc ()
  @ constants ~awsm_service_id service
  @ errors service
  @ constructors ~loc
  @ shape_modules
;;

let structure_multi ~awsm_service_id ~loc index service open_submodules shape_modules =
  match index with
  | 0 ->
    preamble ~loc ()
    @ open_all ~loc open_submodules
    @ constants ~awsm_service_id service
    @ errors service
    @ constructors ~loc
    @ shape_modules
  | _ ->
    (* The subsequent submodules open all of the preceding submodules so
       we don't need to repeat most of the boilerplate. *)
    preamble ~loc () @ open_all ~loc open_submodules @ shape_modules
;;

let module_name_of_ml fn = fn |> String.chop_suffix_exn ~suffix:".ml" |> String.capitalize

let make ~awsm_service_id ~submodules (service : Botodata.service) =
  let loc = !Ast_helper.default_loc in
  let shape_modules = Service_structure.shape_modules service in
  match submodules with
  | [] ->
    let main_module = structure_singleton ~awsm_service_id ~loc service shape_modules in
    let submodules = [] in
    main_module, submodules
  | submodule_fns ->
    let num_submodules = List.length submodule_fns in
    let submodules = List.map submodule_fns ~f:module_name_of_ml in
    let main_module = include_all ~loc submodules in
    let shape_modules_groups =
      let length =
        Float.( / )
          (Float.of_int (List.length shape_modules))
          (Float.of_int num_submodules)
        |> Float.round_up
        |> Float.to_int
      in
      List.chunks_of shape_modules ~length
    in
    let submodules =
      List.mapi submodule_fns ~f:(fun i sub_fn ->
        let sub_mods = List.take submodules i in
        let sub_shapes = List.nth_exn shape_modules_groups i in
        let struct_ =
          structure_multi ~awsm_service_id ~loc i service sub_mods sub_shapes
        in
        sub_fn, struct_)
    in
    main_module, submodules
;;
