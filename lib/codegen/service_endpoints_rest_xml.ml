open! Core
open! Import

let to_request data =
  let loc = !Ast_helper.default_loc in
  let e =
    data
    |> Endpoint.cases ~f:(fun endpoint ->
         match Endpoint.payload endpoint with
         | None -> [%expr Awsm.Http.Request.make (method_of_endpoint endp)]
         | Some payload ->
           Payload.convert_rest_xml
             payload
             ~endpoint_name:(Endpoint.name endpoint)
             [%expr req])
    |> Ast_helper.Exp.match_ [%expr endp]
  in
  let loc = !Ast_helper.default_loc in
  [%stri
    let to_request (type i o e) (endp : (i, o, e) t) (req : i) =
      let _req = req in
      [%e e]
    ;;]
;;

let%expect_test "to_request" =
  let optional_blob =
    Payload.create
      ~is_blob:true
      ~payload_module:"OptionalBlobModule"
      ~field_name:"optional_blob_field"
      ~is_required:false
  in
  let optional_xml =
    Payload.create
      ~is_blob:false
      ~payload_module:"OptionalXmlModule"
      ~field_name:"optional_xml_field"
      ~is_required:false
  in
  let required_blob =
    Payload.create
      ~is_blob:true
      ~payload_module:"RequiredBlobModule"
      ~field_name:"required_blob_field"
      ~is_required:true
  in
  let required_xml =
    Payload.create
      ~is_blob:false
      ~payload_module:"RequiredXmlModule"
      ~field_name:"required_xml_field"
      ~is_required:true
  in
  [ Endpoint.create_test "NoPayload" ~payload:None
  ; Endpoint.create_test "OptionalBlobPayload" ~payload:(Some optional_blob)
  ; Endpoint.create_test "OptionalXmlPayload" ~payload:(Some optional_xml)
  ; Endpoint.create_test "RequiredBlobPayload" ~payload:(Some required_blob)
  ; Endpoint.create_test "RequiredXmlPayload" ~payload:(Some required_xml)
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
      | NoPayload -> Awsm.Http.Request.make (method_of_endpoint endp)
      | OptionalBlobPayload ->
          let body =
            Option.map req.optional_blob_field ~f:OptionalBlobModule.to_header in
          Awsm.Http.Request.make ?body (method_of_endpoint endp)
      | OptionalXmlPayload ->
          let body =
            Option.map req.optional_xml_field
              ~f:(fun param ->
                    (((param |> OptionalXmlModule.to_value) |>
                        (Awsm.Xml.of_value "OptionalXmlPayload"))
                       |> (List.map ~f:Awsm.Xml.to_string))
                      |> (String.concat ~sep:"")) in
          Awsm.Http.Request.make ?body (method_of_endpoint endp)
      | RequiredBlobPayload ->
          let body = RequiredBlobModule.to_header req.required_blob_field in
          Awsm.Http.Request.make ~body (method_of_endpoint endp)
      | RequiredXmlPayload ->
          let body =
            (fun param ->
               (((param |> RequiredXmlModule.to_value) |>
                   (Awsm.Xml.of_value "RequiredXmlPayload"))
                  |> (List.map ~f:Awsm.Xml.to_string))
                 |> (String.concat ~sep:"")) req.required_xml_field in
          Awsm.Http.Request.make ~body (method_of_endpoint endp) |}]
;;

let of_response (service : Botodata.service) data =
  ignore service;
  let loc = !Ast_helper.default_loc in
  let body =
    data
    |> Endpoint.cases ~f:(fun endpoint ->
         let error_of_xml =
           Service_endpoints_common.make_error_expression
             ~loc
             ~label:"error_of_xml"
             endpoint
         in
         match Endpoint.result_decoder endpoint with
         | None -> [%expr return (Ok ())]
         | Some Json -> assert false
         | Some Xml ->
           let of_xml =
             Endpoint.in_result_module endpoint "of_xml"
             |> Option.value_exn
                  ~message:"no result module"
                  ~error:(Error.create_s [%message (endpoint : Endpoint.t)])
           in
           [%expr
             match resp with
             | Error err -> handle_error err [%e error_of_xml]
             | Ok resp -> response_to_xml resp >>| [%e of_xml] >>| ok]
         | Some (Of_header_and_body payload_opt) -> (
           let of_header_and_body =
             Endpoint.in_result_module endpoint "of_header_and_body"
             |> Option.value_exn
                  ~message:"no result module"
                  ~error:(Error.create_s [%message (endpoint : Endpoint.t)])
           in
           match payload_opt with
           | Some payload ->
             let payload =
               let open Option.Let_syntax in
               (let%bind op = Endpoint.op endpoint in
                let%bind op_output = op.output in
                let%bind shape_member =
                  match%bind
                    List.Assoc.find ~equal:String.equal service.shapes op_output.shape
                  with
                  | Structure_shape ss ->
                    List.Assoc.find ~equal:String.equal ss.members payload
                  | _ -> None
                in
                Some shape_member.shape)
               |> Option.value ~default:payload
             in
             let of_string = Ast_convenience.evar (sprintf "%s.of_string" payload) in
             [%expr
               match resp with
               | Error err -> handle_error err [%e error_of_xml]
               | Ok resp ->
                 Awsm.Http.Response.body_to_string state resp
                 >>| [%e of_string]
                 >>= fun body ->
                 let headers =
                   Awsm.Http.Headers.to_list (Awsm.Http.Response.headers resp)
                 in
                 return (Ok ([%e of_header_and_body] (headers, body)))]
           | None ->
             [%expr
               match resp with
               | Error err -> handle_error err [%e error_of_xml]
               | Ok resp ->
                 let headers =
                   Awsm.Http.Headers.to_list (Awsm.Http.Response.headers resp)
                 in
                 return (Ok ([%e of_header_and_body] (headers, ())))]))
    |> Ast_helper.Exp.match_ [%expr endpoint]
  in
  [%stri
    let of_response
      (type s i o e)
      (state : s Awsm.Http.Monad.t)
      (endpoint : (i, o, e) t)
      resp
      : ( (o, [ `AWS of e | `Transport of Awsm.Http.Io.Error.call ]) result, s )
      Awsm.Http.Monad.app
      =
      let ( >>= ) = state.Awsm.Http.Monad.bind in
      let return = state.Awsm.Http.Monad.return in
      let ( >>| ) x f = x >>= fun x -> return (f x) in
      let ok x = Ok x in
      let handle_error err error_of_xml =
        let generic_error () = return (Error (`Transport err)) in
        match err with
        | `Too_many_redirects -> generic_error ()
        | `Bad_response { Awsm.Http.Io.Error.code; body; x_amzn_error_type = _ } -> (
          match error_of_xml, code >= 400 && code <= 599 with
          | None, _ | _, false -> generic_error ()
          | Some error_of_xml, true -> (
            match Awsm.Xml.parse_response body with
            | `Data _ -> generic_error ()
            | `El (((_, "Error"), _), _) as xml -> (
              try
                let error_code =
                  match Awsm.Xml.child_exn xml "Code" with
                  | `Data error_code -> error_code
                  | `El (_, children) ->
                    List.map children ~f:(function
                      | `Data s -> s
                      | `El _ -> "")
                    |> String.concat ~sep:""
                in
                return (Error (`AWS (error_of_xml (String.strip error_code) xml)))
              with
              | Failure _ -> generic_error ())
            | `El _ -> generic_error ()))
      in
      let response_to_xml resp =
        Awsm.Http.Response.body_to_string state resp >>| Awsm.Xml.parse_response
      in
      [%e body]
    ;;]
;;

let%expect_test "of_response" =
  [ Endpoint.create_test
      "Of_header_and_no_body"
      ~result_module:(Some "Result")
      ~result_decoder:(Some (Of_header_and_body None))
  ; Endpoint.create_test
      "Direct"
      ~result_module:(Some "DirectResult")
      ~result_decoder:(Some Xml)
  ; Endpoint.create_test
      "Of_header_and_body"
      ~result_module:(Some "Result_of_header_and_body")
      ~result_decoder:(Some (Of_header_and_body (Some "Payload_module")))
  ; Endpoint.create_test "No_output" ~result_module:None ~result_decoder:None
  ]
  |> of_response
       { metadata = Botodata.empty_metadata_for_tests
       ; documentation = None
       ; version = None
       ; operations = []
       ; shapes = []
       }
  |> List.return
  |> Util.structure_to_string
  |> printf "%s%!";
  [%expect
    {|
    let of_response (type s) (type i) (type o) (type e)
      (state : s Awsm.Http.Monad.t) (endpoint : (i, o, e) t) resp =
      (let (>>=) = state.Awsm.Http.Monad.bind in
       let return = state.Awsm.Http.Monad.return in
       let (>>|) x f = x >>= (fun x -> return (f x)) in
       let ok x = Ok x in
       let handle_error err error_of_xml =
         let generic_error () = return (Error (`Transport err)) in
         match err with
         | `Too_many_redirects -> generic_error ()
         | `Bad_response
             { Awsm.Http.Io.Error.code = code; body; x_amzn_error_type = _ } ->
             (match (error_of_xml, ((code >= 400) && (code <= 599))) with
              | (None, _) | (_, false) -> generic_error ()
              | (Some error_of_xml, true) ->
                  (match Awsm.Xml.parse_response body with
                   | `Data _ -> generic_error ()
                   | `El (((_, "Error"), _), _) as xml ->
                       (try
                          let error_code =
                            match Awsm.Xml.child_exn xml "Code" with
                            | `Data error_code -> error_code
                            | `El (_, children) ->
                                (List.map children
                                   ~f:(function | `Data s -> s | `El _ -> ""))
                                  |> (String.concat ~sep:"") in
                          return
                            (Error
                               (`AWS (error_of_xml (String.strip error_code) xml)))
                        with | Failure _ -> generic_error ())
                   | `El _ -> generic_error ())) in
       let response_to_xml resp =
         (Awsm.Http.Response.body_to_string state resp) >>|
           Awsm.Xml.parse_response in
       match endpoint with
       | Of_header_and_no_body ->
           (match resp with
            | Error err -> handle_error err None
            | Ok resp ->
                let headers =
                  Awsm.Http.Headers.to_list (Awsm.Http.Response.headers resp) in
                return (Ok (Result.of_header_and_body (headers, ()))))
       | Direct ->
           (match resp with
            | Error err -> handle_error err None
            | Ok resp -> ((response_to_xml resp) >>| DirectResult.of_xml) >>| ok)
       | Of_header_and_body ->
           (match resp with
            | Error err -> handle_error err None
            | Ok resp ->
                ((Awsm.Http.Response.body_to_string state resp) >>|
                   Payload_module.of_string)
                  >>=
                  ((fun body ->
                      let headers =
                        Awsm.Http.Headers.to_list
                          (Awsm.Http.Response.headers resp) in
                      return
                        (Ok
                           (Result_of_header_and_body.of_header_and_body
                              (headers, body))))))
       | No_output -> return (Ok ()) : ((o,
                                          [ `AWS of e
                                          | `Transport of Awsm.Http.Io.Error.call ])
                                          result,
                                         s) Awsm.Http.Monad.app) |}]
;;

let make_structure_for_protocol service data =
  [ to_request data; of_response service data ]
;;
