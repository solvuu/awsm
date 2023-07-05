open! Core
open! Import

type member_info =
  { required : bool
  ; arg_name : string
  ; shape_name : string
  ; member_shape : Botodata.shape
  }

let to_flag_name s =
  s
  |> Util.camel_to_snake_case ~sep:'-'
  |> String.map ~f:(function
       | '_' -> '-'
       | c -> c)
;;

let member_to_flag ~loc { required; arg_name; member_shape; shape_name } =
  let flag_name = to_flag_name arg_name in
  let required_or_optional =
    match required with
    | true -> [%expr required]
    | false -> [%expr optional]
  in
  let arg_type, doc =
    match member_shape with
    | Boolean_shape _ -> [%expr bool], "BOOL"
    | Integer_shape _ -> [%expr int], "INT"
    | Float_shape _ | Double_shape _ -> [%expr float], "FLOAT"
    | String_shape _ -> [%expr string], "STRING"
    | _ -> [%expr json_arg], "JSON"
  in
  [%expr
    flag
      [%e Ast_convenience.str flag_name]
      ([%e required_or_optional] [%e arg_type])
      ~doc:[%e Ast_convenience.str (doc ^ " " ^ shape_name)]]
;;

let member_to_arg ~loc { required; arg_name = name; member_shape; shape_name } =
  let name_expr =
    match required with
    | true -> Ast_convenience.Label.labelled name
    | false -> Ast_convenience.Label.optional name
  in
  let value_expr =
    match member_shape with
    | Boolean_shape _ | Integer_shape _ | Float_shape _ | Double_shape _ | String_shape _
      -> [%expr [%e Ast_convenience.evar name]]
    | _ -> (
      let shape_name = Shape.capitalized_id shape_name in
      let conv = sprintf "Values.%s.of_json" shape_name |> Ast_convenience.evar in
      match required with
      | true -> [%expr [%e conv] [%e Ast_convenience.evar name]]
      | false -> [%expr Option.map ~f:[%e conv] [%e Ast_convenience.evar name]])
  in
  name_expr, value_expr
;;

let make_structure_for_protocol
  ~loc
  service
  (metadata : Botodata.metadata)
  (data : Endpoint.t list)
  =
  let shape_map = String.Map.of_alist_exn service.Botodata.shapes in
  List.filter_map data ~f:(fun e ->
    let name = Endpoint.name e in
    let request_shape = Endpoint.request_module e in
    let error_to_sexp =
      match metadata.protocol with
      | `ec2 -> [%expr Some Values.Ec2_error.sexp_of_t]
      | `json | `query | `rest_xml | `rest_json -> (
        match Endpoint.result_module e with
        | None -> [%expr None]
        | Some result_shape ->
          let f =
            sprintf "Values.%s.sexp_of_error" result_shape |> Ast_convenience.evar
          in
          [%expr Some [%e f]])
    in
    let result_to_json =
      match Endpoint.result_module e with
      | None -> [%expr None]
      | Some result_shape ->
        let f = sprintf "Values.%s.to_json" result_shape |> Ast_convenience.evar in
        [%expr Some [%e f]]
    in
    let name_unders = Util.camel_to_snake_case name in
    let io_fun = sprintf "Io.%s" name_unders |> Ast_convenience.evar in
    let make_fun =
      (match request_shape with
       | None -> sprintf "Fn.id"
       | Some request_shape -> sprintf "Values.%s.make" request_shape)
      |> Ast_convenience.evar
    in
    let make_args =
      match request_shape with
      | None -> []
      | Some request_shape -> (
        match String.Map.find shape_map request_shape with
        | None -> failwithf "could not find %s in shape map" request_shape ()
        | Some (Structure_shape ss) ->
          List.map ss.members ~f:(fun (field_name, member) ->
            { required = Shape.structure_shape_required_field ss field_name
            ; arg_name = Shape.uncapitalized_id field_name
            ; shape_name = member.shape
            ; member_shape = String.Map.find_exn shape_map member.shape
            })
          |> List.stable_sort ~compare:(fun x y -> Bool.compare x.required y.required)
        | Some _ -> failwithf "request shape %s is not a structure shape" request_shape ()
        )
    in
    let let_and_body =
      let vbs =
        match make_args with
        | [] ->
          [ Ast_helper.Vb.mk
              (Ast_helper.Pat.var (Ast_convenience.mknoloc "()"))
              [%expr return ()]
          ]
        | make_args ->
          List.map make_args ~f:(fun member_info ->
            Ast_helper.Vb.mk
              (Ast_helper.Pat.var (Ast_convenience.mknoloc member_info.arg_name))
              (member_to_flag ~loc member_info))
      in
      let common_args =
        let mk name fla doc =
          Ast_helper.Vb.mk
            (Ast_helper.Pat.var (Ast_convenience.mknoloc name))
            [%expr
              flag
                [%e Ast_convenience.str fla]
                (optional string)
                ~doc:[%e Ast_convenience.str doc]]
        in
        [ mk "cli_profile" "-cli-profile" "NAME aws profile to use"
        ; mk "cli_region" "-cli-region" "REGION override region"
        ; mk "endpoint_url" "-endpoint-url" "URL override endpoint url"
        ]
      in
      Ast_helper.Exp.let_
        Nonrecursive
        (common_args @ vbs)
        [%expr
          fun () ->
            call
              ?endpoint_url
              ?profile:cli_profile
              ?region:cli_region
              [%e io_fun]
              [%e
                Ast_convenience.app_labels
                  make_fun
                  (List.map make_args ~f:(member_to_arg ~loc)
                  @ [ Ast_convenience.Label.nolabel, Ast_convenience.unit () ])]
              [%e result_to_json]
              [%e error_to_sexp]]
    in
    Some
      [%stri
        let [%p Ast_convenience.pvar name_unders] =
          Command.async ~summary:"" [%map_open.Command [%e let_and_body]]
        ;;])
;;

let command_main ~loc data =
  let command_list =
    List.map data ~f:(fun e ->
      let name = Endpoint.name e in
      let name_dashed = Util.camel_to_snake_case ~sep:'-' name in
      let name_unders = Util.camel_to_snake_case name in
      Ast_convenience.tuple
        [ Ast_convenience.str name_dashed; Ast_convenience.evar name_unders ])
    |> Ast_convenience.list
  in
  [%str
    let main =
      Command.group
        ~summary:(Awsm_codegen.Service.to_string Values.service ^ " commands")
        [%e command_list]
    ;;]
;;

let preamble ~loc =
  [%str
    let json_arg = Command.Arg_type.create Awsm_codegen.Json.from_string

    let call ?endpoint_url ?profile ?region f m result_to_json error_to_sexp =
      let region =
        match region with
        | Some region -> Some (Awsm.Region.of_string region)
        | None -> None
      in
      Awsm_async.Cfg.get_exn ?profile ?region ()
      >>= fun cfg ->
      f (Awsm_async.Http.Io.call ?endpoint_url ~service:Values.service ~cfg) m
      >>= fun result ->
      match result with
      | Error err -> (
        match err with
        | `Transport err ->
          failwiths ~here:[%here] "Transport error" err Awsm.Http.Io.Error.sexp_of_call
        | `AWS err -> (
          match error_to_sexp with
          | None -> failwithf "endpoint error, but no error values defined in boto" ()
          | Some to_sexp -> failwiths ~here:[%here] "AWS error" err to_sexp))
      | Ok result ->
        (match result_to_json with
         | None -> print_endline "ok response from endpoint"
         | Some to_json ->
           result |> to_json |> Awsm_codegen.Json.to_string |> print_endline);
        return ()
    ;;]
;;

let structure_singleton ~loc service metadata data =
  preamble ~loc
  @ make_structure_for_protocol ~loc service metadata data
  @ command_main ~loc data
;;

let mod_of_str s = s |> Ast_convenience.lid |> Ast_helper.Mod.ident

let open_all ~loc =
  List.map ~f:(fun s -> [%stri open [%m mod_of_str s] [@@warning "-33"]])
;;

let include_all ~loc = List.map ~f:(fun s -> [%stri include [%m mod_of_str s]])

let structure_multi ~loc index service open_submodules metadata data =
  match index with
  | 0 ->
    preamble ~loc
    @ open_all ~loc open_submodules
    @ make_structure_for_protocol ~loc service metadata data
  | _ ->
    (* The subsequent submodules open all of the preceding submodules so
       we don't need to repeat most of the boilerplate. *)
    open_all ~loc open_submodules @ make_structure_for_protocol ~loc service metadata data
;;

let module_name_of_ml fn = fn |> String.chop_suffix_exn ~suffix:".ml" |> String.capitalize

let make ~submodules (service : Botodata.service) =
  let loc = !Ast_helper.default_loc in
  let endpoints = service.operations |> List.map ~f:(Endpoint.of_botodata ~service) in
  match submodules with
  | [] ->
    let main_module = structure_singleton ~loc service service.metadata endpoints in
    let submodules = [] in
    main_module, submodules
  | submodule_fns ->
    let num_submodules = List.length submodule_fns in
    let submodules = List.map submodule_fns ~f:module_name_of_ml in
    let main_module = include_all ~loc submodules @ command_main ~loc endpoints in
    let endpoints_groups =
      let length =
        Float.( / ) (Float.of_int (List.length endpoints)) (Float.of_int num_submodules)
        |> Float.round_up
        |> Float.to_int
      in
      List.chunks_of endpoints ~length
    in
    let submodules =
      List.mapi submodule_fns ~f:(fun i sub_fn ->
        let sub_mods = List.take submodules i in
        let sub_shapes = List.nth_exn endpoints_groups i in
        sub_fn, structure_multi ~loc i service sub_mods service.metadata sub_shapes)
    in
    main_module, submodules
;;
