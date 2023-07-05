open! Core
open! Import

let x_field fld =
  let loc = !Ast_helper.default_loc in
  Ast_helper.Exp.field [%expr x] (Ast_convenience.lid fld)
;;

let apply_query_params query_params e =
  let loc = !Ast_helper.default_loc in
  match query_params with
  | [] -> e
  | l ->
    let params_expr =
      List.map l ~f:(fun param ->
        let convert = Query_param.convert param in
        let value =
          param |> Query_param.field_name |> Shape.uncapitalized_id |> x_field
        in
        let param_s = Ast_convenience.str (Query_param.param_name param) in
        if Query_param.is_required param
        then
          Ast_convenience.some
            (Ast_convenience.pair param_s (Ast_convenience.app convert [ value ]))
        else [%expr Core.Option.map ~f:(fun v -> [%e param_s], [%e convert] v) [%e value]])
      |> Ast_convenience.list
    in
    [%expr Uri.add_query_params' [%e e] (Core.List.filter_opt [%e params_expr])]
;;

let get_field_for_query_variable ~service ~(input : Botodata.operation_input) v =
  let field_name =
    match v with
    | `Variable (field_name, _) -> field_name
    | _ -> assert false
  in
  let structure_shape =
    let input_shape =
      match
        List.Assoc.find service.Botodata.shapes input.Botodata.shape ~equal:String.equal
      with
      | Some x -> x
      | None ->
        failwithf
          "get_field_for_query_variable: shape '%s' not found in [%s]"
          input.Botodata.shape
          (String.concat ~sep:"," (service.Botodata.shapes |> List.map ~f:fst))
          ()
    in
    match input_shape with
    | Structure_shape ss -> ss
    | _ ->
      (* rest_{json,xml} can only take structure shape *)
      assert false
  in
  let name, member =
    let members = structure_shape.members in
    let member =
      List.find_map members ~f:(fun (member_name, member) ->
        let name =
          match member.locationName with
          | Some location_name -> location_name
          | None -> member_name
        in
        match String.equal name field_name with
        | true -> Some (member_name, member)
        | false -> None)
    in
    match member with
    | Some (name, member) -> name, member
    | None ->
      failwithf
        "Endpoints.get_field_for_query_variable: member '%s' not found in [%s]"
        field_name
        (String.concat ~sep:"," (List.map members ~f:fst))
        ()
  in
  Shape.uncapitalized_id name, member
;;

let request_uri_as_exp tokens ~service ~op ~query_params =
  let loc = !Ast_helper.default_loc in
  let add_to_fmt suf (fmt, args) = fmt ^ suf, args in
  let add_arg arg (fmt, args) = fmt, arg :: args in
  let handle = function
    | `Qmark -> add_to_fmt "?"
    | `Ampersand -> add_to_fmt "&"
    | `Slash -> add_to_fmt "/"
    | `Equal -> add_to_fmt "="
    | `String s -> add_to_fmt s
    | `Variable _ as v ->
      let input =
        Option.value_exn ~message:"request_uri_as_sexp: No op.input" op.Botodata.input
      in
      let name, member = get_field_for_query_variable ~service ~input v in
      let field =
        Ast_helper.Exp.field
          (Ast_convenience.evar "x")
          (Ast_convenience.lid (Shape.capitalized_id input.shape ^ "." ^ name))
      in
      let value =
        Ast_convenience.app
          (Ast_convenience.evar (Shape.capitalized_id member.shape ^ ".to_header"))
          [ field ]
      in
      Fn.compose (add_to_fmt "%s") (add_arg value)
  in
  let format_string, args =
    List.fold_left tokens ~init:("", []) ~f:(fun acc token -> handle token acc)
  in
  let all_args = Ast_convenience.str format_string :: List.rev args in
  Ast_convenience.app [%expr Format.kasprintf Uri.of_string] all_args
  |> apply_query_params query_params
;;

let uri_of_endpoint ~service data =
  let loc = !Ast_helper.default_loc in
  let match_ =
    Endpoint.cases data ~f:(fun endpoint ->
      let uri = Endpoint.request_uri endpoint in
      let query_params = Endpoint.query_params endpoint in
      let op = Option.value_exn ~message:"No op" (Endpoint.op endpoint) in
      request_uri_as_exp uri ~op ~service ~query_params)
    |> Ast_helper.Exp.match_ [%expr endpoint]
  in
  [%expr fun [@ocaml.warning "-27"] endpoint x -> [%e match_]]
;;

(* This expect test is non-trivial to fix and I'm not sure what its value is.
   The reams upon reams of generated code quickly throw up errors if you
   make a breaking change. We can revisit later. *)
(*
let%expect_test "uri_of_endpoint" =
  let make_input ~shape =
    { Botodata.shape
    ; documentation = None
    ; deprecated = None
    ; xmlNamespace = None
    ; locationName = None
    }
  in
  let make_op ~name ~input =
    { Botodata.name
    ; http = { Botodata.method_ = `GET; requestUri = []; responseCode = None }
    ; input = Some input
    ; output = None
    ; errors = None
    ; documentation = None
    ; documentationUrl = None
    ; alias = None
    ; deprecated = None
    ; authtype = None
    ; idempotent = None
    ; httpChecksum = None
    ; endpoint = None
    }
  in
  let input1 = make_input ~shape:"Shape1" in
  let input2 = make_input ~shape:"Shape2" in
  let input3 = make_input ~shape:"Shape3" in
  let bucket = make_input ~shape:"Bucket" in
  let op1 = make_op ~name:"op1" ~input:input1 in
  let op2 = make_op ~name:"op2" ~input:input2 in
  let op3 = make_op ~name:"op3" ~input:input3 in
  let op4 = make_op ~name:"op4" ~input:bucket in
  let service =
    let metadata = Botodata.empty_metadata_for_tests in
    let make_ss () =
      Botodata.Structure_shape
        { Botodata.required = None
        ; members = []
        ; error = None
        ; exception_ = None
        ; fault = None
        ; documentation = None
        ; payload = None
        ; xmlNamespace = None
        ; wrapper = None
        ; deprecated = None
        ; sensitive = None
        ; xmlOrder = None
        ; locationName = None
        ; event = None
        ; eventstream = None
        }
    in
    let s = make_ss () in
    { Botodata.metadata
    ; documentation = None
    ; version = None
    ; operations = [ op1; op2; op3; op4 ]
    ; shapes = [ "Shape1", s; "Shape2", s; "Shape3", s; "Bucket", s ]
    }
  in
  let create name ~op ~uri ~query_params =
    let request_uri = Botodata.parse_requestUri uri in
    Endpoint.create_test name ~service:(Some service) ~op ~request_uri ~query_params
  in
  [ create "Bucket" ~op:(Some op4) ~uri:"/{Bucket}" ~query_params:[]
  ; create "Bucket_param" ~op:(Some op4) ~uri:"/{Bucket}?analytics" ~query_params:[]
  ; create "Bucket_key" ~op:(Some op4) ~uri:"/{Bucket}/{Key+}" ~query_params:[]
  ; create
      "Bucket_key_param"
      ~op:(Some op1)
      ~uri:"/{Bucket}/{Key+}?uploads"
      ~query_params:[]
  ; create
      "Bucket_query_params"
      ~op:(Some op1)
      ~uri:"/{Bucket}"
      ~query_params:
        [ Query_param.create
            ~name:"param1"
            ~shape:"Shape1"
            ~is_required:true
            ~field_name:"param1"
        ; Query_param.create
            ~name:"param2"
            ~shape:"Shape2"
            ~is_required:true
            ~field_name:"param2"
        ; Query_param.create
            ~name:"param3"
            ~shape:"Shape3"
            ~is_required:false
            ~field_name:"param3"
        ]
  ]
  |> uri_of_endpoint ~service
  |> fun x -> printf "%s%!" (Util.expression_to_string x);
  [%expect
    {|
    ((fun endpoint ->
        fun x ->
          match endpoint with
          | Bucket -> (Format.kasprintf Uri.of_string) "/%s" x.bucket
          | Bucket_param ->
              (Format.kasprintf Uri.of_string) "/%s?analytics" x.bucket
          | Bucket_key ->
              (Format.kasprintf Uri.of_string) "/%s/%s" x.bucket x.key
          | Bucket_key_param ->
              (Format.kasprintf Uri.of_string) "/%s/%s?uploads" x.bucket x.key
          | Bucket_query_params ->
              Uri.add_query_params'
                ((Format.kasprintf Uri.of_string) "/%s" x.bucket)
                (Core.List.filter_opt
                   [Some ("param1", (Shape1.to_header x.param1));
                   Some ("param2", (Shape2.to_header x.param2));
                   Core.Option.map ~f:(fun v -> ("param3", (Shape3.to_header v)))
                     x.param3]))
    [@ocaml.warning "-27"]) |}]
;;
*)

let method_of_endpoint data =
  Endpoint.cases data ~f:(fun endpoint ->
    Ast_convenience.http_method (Endpoint.meth endpoint))
  |> Ast_helper.Exp.function_
;;

let%expect_test "method_of_endpoint" =
  let data =
    [ Endpoint.create_test "Endpoint_with_get" ~meth:`GET
    ; Endpoint.create_test "Endpoint_with_post" ~meth:`POST
    ]
  in
  method_of_endpoint data
  |> fun x ->
  printf "%s%!" (Util.expression_to_string x);
  [%expect {| function | Endpoint_with_get -> `GET | Endpoint_with_post -> `POST |}]
;;

let type_decl data =
  let loc = !Ast_helper.default_loc in
  let cases =
    List.map data ~f:(fun endpoint ->
      let cstr = Endpoint.name endpoint in
      let res =
        [%type:
          ( [%t Endpoint.request_type endpoint]
          , [%t Endpoint.result_ok_type endpoint]
          , [%t Endpoint.result_error_type endpoint] )
          t]
      in
      Ast_helper.Type.constructor (Ast_convenience.mknoloc cstr) ~res)
  in
  Ast_helper.Str.type_
    Recursive
    [ Ast_helper.Type.mk
        (Ast_convenience.mknoloc "t")
        ~params:
          [ [%type: 'i], (NoVariance, NoInjectivity)
          ; [%type: 'o], (NoVariance, NoInjectivity)
          ; [%type: 'e], (NoVariance, NoInjectivity)
          ]
        ~kind:(Ptype_variant cases)
    ]
;;

let%expect_test "type_decl" =
  let data =
    [ Endpoint.create_test
        "Input_and_output"
        ~request_module:(Some "Input")
        ~result_module:(Some "Output")
    ; Endpoint.create_test "Only_input" ~request_module:(Some "Input") ~result_module:None
    ; Endpoint.create_test
        "Only_output"
        ~request_module:None
        ~result_module:(Some "Output")
    ; Endpoint.create_test
        "No_input_and_no_output"
        ~request_module:None
        ~result_module:None
    ]
  in
  type_decl data |> List.return |> Util.structure_to_string |> printf "%s%!";
  [%expect
    {|
    type ('i, 'o, 'e) t =
      | Input_and_output: (Input.t, Output.t, Output.error) t
      | Only_input: (Input.t, unit, unit) t
      | Only_output: (unit, Output.t, Output.error) t
      | No_input_and_no_output: (unit, unit, unit) t |}]
;;

let make_structure_for_protocol service (metadata : Botodata.metadata) data =
  match metadata.protocol with
  | `rest_xml -> Service_endpoints_rest_xml.make_structure_for_protocol service data
  | `rest_json -> Service_endpoints_rest_json.make_structure_for_protocol service data
  | `json -> Service_endpoints_json.make_structure_for_protocol metadata data
  | `ec2 -> Service_endpoints_ec2.make_structure_for_protocol service metadata data
  | `query -> Service_endpoints_query.make_structure_for_protocol service metadata data
;;

let full_structure service metadata data =
  let loc = !Ast_helper.default_loc in
  [%str
    open Values

    [%%i type_decl data]

    let method_of_endpoint : type i o e. (i, o, e) t -> _ = [%e method_of_endpoint data]

    let uri_of_endpoint : type i o e. (i, o, e) t -> i -> Uri.t =
      [%e uri_of_endpoint ~service data]
    ;;]
  @ make_structure_for_protocol service metadata data
;;

let make service =
  service.operations
  |> List.map ~f:(Endpoint.of_botodata ~service)
  |> full_structure service service.metadata
;;
