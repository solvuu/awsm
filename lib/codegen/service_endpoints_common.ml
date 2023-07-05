open! Core
open! Import

let request_body_arg (op : Botodata.operation option) =
  let op = Option.value_exn ~message:"No op?" op in
  let action = op.name in
  let loc = !Ast_helper.default_loc in
  match op.input with
  | None -> [%expr None]
  | Some input ->
    [%expr
      let meta =
        [ "Action", [ [%e Ast_convenience.str action] ]; "Version", [ apiVersion ] ]
      in
      let query =
        [%e Ast_convenience.evar (Shape.capitalized_id input.shape ^ ".to_query")] req
        |> Awsm.Client.Query.render
      in
      Some (Uri.encoded_of_query (meta @ query))]
;;

let to_request endpoints =
  let loc = !Ast_helper.default_loc in
  let expr =
    Endpoint.cases endpoints ~f:(fun endpoint ->
      [%expr
        let headers =
          Awsm.Http.Headers.of_list
            [ "content-type", "application/x-www-form-urlencoded; charset=utf-8" ]
        in
        let body = [%e request_body_arg (Endpoint.op endpoint)] in
        Awsm.Http.Request.make ?body ~headers (method_of_endpoint endp)])
    |> Ast_helper.Exp.match_ [%expr endp]
  in
  [%stri
    let to_request (type i o e) (endp : (i, o, e) t) (req : i) =
      let _req = req in
      [%e expr]
    ;;]
;;

let%expect_test "to_request" =
  let make_op ~name ~input_shape =
    let make_input ~shape =
      { Botodata.shape
      ; documentation = None
      ; deprecated = None
      ; xmlNamespace = None
      ; locationName = None
      }
    in
    Some
      { Botodata.name
      ; http = { Botodata.method_ = `GET; requestUri = []; responseCode = None }
      ; input = Some (make_input ~shape:input_shape)
      ; output = None
      ; errors = None
      ; documentation = None
      ; documentationUrl = None
      ; alias = None
      ; deprecated = None
      ; deprecatedMessage = None
      ; authtype = None
      ; idempotent = None
      ; httpChecksum = None
      ; endpoint = None
      ; endpointdiscovery = None
      }
  in
  [ Endpoint.create_test
      "Name1"
      ~op:(make_op ~name:"foo" ~input_shape:"Name1")
      ~request_module:(Some "Module1")
  ; Endpoint.create_test
      "Name2"
      ~op:(make_op ~name:"bar" ~input_shape:"Name2")
      ~request_module:(Some "Module2")
  ; Endpoint.create_test
      "Name3"
      ~op:(make_op ~name:"baz" ~input_shape:"Name3")
      ~request_module:None
  ]
  |> to_request
  |> List.return
  |> Util.structure_to_string
  |> printf "%s%!";
  [%expect
    {|
    let to_request (type i) (type o) (type e) (endp : (i, o, e) t) (req : i) =
      let _req = req in
      match endp with
      | Name1 ->
          let headers =
            Awsm.Http.Headers.of_list
              [("content-type",
                 "application/x-www-form-urlencoded; charset=utf-8")] in
          let body =
            let meta = [("Action", ["foo"]); ("Version", [apiVersion])] in
            let query = (Name1.to_query req) |> Awsm.Client.Query.render in
            Some (Uri.encoded_of_query (meta @ query)) in
          Awsm.Http.Request.make ?body ~headers (method_of_endpoint endp)
      | Name2 ->
          let headers =
            Awsm.Http.Headers.of_list
              [("content-type",
                 "application/x-www-form-urlencoded; charset=utf-8")] in
          let body =
            let meta = [("Action", ["bar"]); ("Version", [apiVersion])] in
            let query = (Name2.to_query req) |> Awsm.Client.Query.render in
            Some (Uri.encoded_of_query (meta @ query)) in
          Awsm.Http.Request.make ?body ~headers (method_of_endpoint endp)
      | Name3 ->
          let headers =
            Awsm.Http.Headers.of_list
              [("content-type",
                 "application/x-www-form-urlencoded; charset=utf-8")] in
          let body =
            let meta = [("Action", ["baz"]); ("Version", [apiVersion])] in
            let query = (Name3.to_query req) |> Awsm.Client.Query.render in
            Some (Uri.encoded_of_query (meta @ query)) in
          Awsm.Http.Request.make ?body ~headers (method_of_endpoint endp) |}]
;;

let make_error_expression ~loc ~label endpoint =
  match Endpoint.op endpoint with
  | None -> [%expr None]
  | Some operation -> (
    match operation.output, operation.errors with
    | None, _ | _, None -> [%expr None]
    | Some _, Some _ ->
      let to_error =
        Endpoint.in_result_module endpoint label
        |> Option.value_exn
             ~message:"no result module"
             ~error:(Error.create_s [%message (endpoint : Endpoint.t)])
      in
      [%expr Some [%e to_error]])
;;
