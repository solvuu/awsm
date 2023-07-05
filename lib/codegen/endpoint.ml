open! Core
open! Import

type t =
  { name : string
  ; op : Botodata.operation option
  ; request_module : string option
  ; result_module : string option
  ; meth : Botodata.http_method
  ; request_uri : Botodata.requestUri
  ; query_params : Query_param.t list
  ; payload : Payload.t option
  ; result_decoder : Result_decoder.t option
  }
[@@deriving fields, sexp_of]

let create = Fields.create
let request_module t = t.request_module
let result_module t = t.result_module

let create_test
  ?(op = None)
  ?(request_module = None)
  ?(result_module = None)
  ?(meth = `GET)
  ?(request_uri = [])
  ?(query_params = [])
  ?(payload = None)
  ?(result_decoder = None)
  name
  =
  create
    ~op
    ~request_module
    ~result_module
    ~meth
    ~request_uri
    ~query_params
    ~payload
    ~name
    ~result_decoder
;;

let lid_in_module mod_ id =
  Option.map mod_ ~f:(fun s -> Printf.ksprintf Ast_convenience.lid "%s.%s" s id)
;;

let expr_in_module mod_ id =
  Option.map (lid_in_module mod_ id) ~f:(fun lid -> Ast_helper.Exp.construct lid None)
;;

let mod_x x mod_ =
  let loc = !Ast_helper.default_loc in
  match lid_in_module mod_ x with
  | Some lid -> Ast_helper.Typ.constr lid []
  | None -> [%type: unit]
;;

let mod_t mod_ = mod_x "t" mod_
let mod_error mod_ = mod_x "error" mod_
let request_type e = mod_t e.request_module
let result_ok_type e = mod_t e.result_module
let result_error_type e = mod_error e.result_module
let in_result_module e id = expr_in_module e.result_module id
let in_request_module e id = expr_in_module e.request_module id

let of_botodata (op : Botodata.operation) ~service =
  let shapes = service.Botodata.shapes in
  let request_module = Option.map ~f:(fun input -> input.shape) op.input in
  let result_module = Option.map ~f:(fun output -> output.shape) op.output in
  let meth = op.http.method_ in
  let request_uri = op.http.requestUri in
  let query_params = Query_param.of_botodata op ~shapes in
  let payload = Payload.of_botodata op ~shapes in
  let result_decoder =
    match service.metadata.protocol with
    | `rest_xml -> Result_decoder.of_botodata_xml op ~shapes
    | `rest_json -> Result_decoder.of_botodata_json op ~shapes
    | _proto -> None
  in
  { name = op.name
  ; op = Some op
  ; request_module
  ; result_module
  ; meth
  ; request_uri
  ; query_params
  ; payload
  ; result_decoder
  }
;;

let cases ~f data =
  List.map data ~f:(fun endpoint ->
    let name = name endpoint in
    let pat = Ast_helper.Pat.construct (Ast_convenience.lid name) None in
    let expr = f endpoint in
    Ast_helper.Exp.case pat expr)
;;
