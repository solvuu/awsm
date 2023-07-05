open! Core
open! Import

let to_request = Service_endpoints_common.to_request

let of_response endpoints =
  let loc = !Ast_helper.default_loc in
  let body =
    Endpoint.cases endpoints ~f:(fun e ->
      match Endpoint.in_result_module e "of_xml" with
      | None -> [%expr response_of_none ()]
      | Some of_xml -> [%expr response_of_some_xml [%e of_xml]])
    |> Ast_helper.Exp.match_ [%expr endpoint]
  in
  [%stri
    let of_response
      (type s i o e)
      (state : s Awsm.Http.Monad.t)
      (endpoint : (i, o, e) t)
      resp
      : ( (o, [ `AWS of Ec2_error.t | `Transport of Awsm.Http.Io.Error.call ]) result, s
      ) Awsm.Http.Monad.app
      =
      let ( >>= ) = state.Awsm.Http.Monad.bind in
      let return = state.Awsm.Http.Monad.return in
      let response_of_error err =
        match err with
        | `Too_many_redirects -> return (Error (`Transport `Too_many_redirects))
        | `Bad_response { Awsm.Http.Io.Error.code; body; x_amzn_error_type } ->
          if code >= 400 && code <= 599
          then (
            let xml = Awsm.Xml.parse_response body in
            return (Error (`AWS (Ec2_error.of_xml xml))))
          else
            return
              (Error
                 (`Transport
                   (`Bad_response { Awsm.Http.Io.Error.code; body; x_amzn_error_type })))
      in
      let response_of_none () =
        match resp with
        | Error err -> response_of_error err
        | Ok _ -> return (Ok ())
      in
      let response_of_some_xml of_xml =
        match resp with
        | Error err -> response_of_error err
        | Ok resp ->
          Awsm.Http.Response.body_to_string state resp
          >>= fun xmls ->
          let xml = Awsm.Xml.parse_response xmls in
          return (Ok (of_xml xml))
      in
      [%e body]
    ;;]
;;

let%expect_test "of_response" =
  [ Endpoint.create_test "Name1" ~result_module:(Some "ResultModule1")
  ; Endpoint.create_test "Name2" ~result_module:(Some "ResultModule2")
  ; Endpoint.create_test "Name3" ~result_module:None
  ]
  |> of_response
  |> List.return
  |> Util.structure_to_string
  |> printf "%s%!";
  [%expect
    {|
    let of_response (type s) (type i) (type o) (type e)
      (state : s Awsm.Http.Monad.t) (endpoint : (i, o, e) t) resp =
      (let (>>=) = state.Awsm.Http.Monad.bind in
       let return = state.Awsm.Http.Monad.return in
       let response_of_error err =
         match err with
         | `Too_many_redirects -> return (Error (`Transport `Too_many_redirects))
         | `Bad_response
             { Awsm.Http.Io.Error.code = code; body; x_amzn_error_type } ->
             if (code >= 400) && (code <= 599)
             then
               let xml = Awsm.Xml.parse_response body in
               return (Error (`AWS (Ec2_error.of_xml xml)))
             else
               return
                 (Error
                    (`Transport
                       (`Bad_response
                          {
                            Awsm.Http.Io.Error.code = code;
                            body;
                            x_amzn_error_type
                          }))) in
       let response_of_none () =
         match resp with
         | Error err -> response_of_error err
         | Ok _ -> return (Ok ()) in
       let response_of_some_xml of_xml =
         match resp with
         | Error err -> response_of_error err
         | Ok resp ->
             (Awsm.Http.Response.body_to_string state resp) >>=
               ((fun xmls ->
                   let xml = Awsm.Xml.parse_response xmls in
                   return (Ok (of_xml xml)))) in
       match endpoint with
       | Name1 -> response_of_some_xml ResultModule1.of_xml
       | Name2 -> response_of_some_xml ResultModule2.of_xml
       | Name3 -> response_of_none () : ((o,
                                           [ `AWS of Ec2_error.t
                                           | `Transport of
                                               Awsm.Http.Io.Error.call ])
                                           result,
                                          s) Awsm.Http.Monad.app) |}]
;;

let make_structure_for_protocol _service _metadata data =
  [ to_request data ] @ [ of_response data ]
;;
