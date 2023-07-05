open! Core
open! Import

let to_request = Service_endpoints_common.to_request

let of_response endpoints =
  let loc = !Ast_helper.default_loc in
  let body =
    Endpoint.cases endpoints ~f:(fun endpoint ->
      let error_of_xml =
        Service_endpoints_common.make_error_expression ~loc ~label:"error_of_xml" endpoint
      in
      match Endpoint.in_result_module endpoint "of_xml" with
      | None -> [%expr return (Ok ())]
      | Some of_xml ->
        [%expr
          match resp with
          | Error err -> handle_error err [%e error_of_xml]
          | Ok resp ->
            Awsm.Http.Response.body_to_string state resp
            >>= fun xmls ->
            let xml = Awsm.Xml.parse_response xmls in
            return (Ok ([%e of_xml] xml))])
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
      let handle_error err error_of_xml =
        (* FIXME: this error handling pattern appears over and over again. factor out. *)
        let generic_error () = return (Error (`Transport err)) in
        match err with
        | `Too_many_redirects -> generic_error ()
        | `Bad_response { Awsm.Http.Io.Error.code; body; x_amzn_error_type = _ } -> (
          match error_of_xml, code >= 400 && code <= 599 with
          | None, _ | _, false -> generic_error ()
          | Some error_of_xml, true -> (
            match Awsm.Xml.parse_response body with
            | `Data _ -> generic_error ()
            | `El (((_, "ErrorResponse"), _), _) as error_response_xml -> (
              let error_xml = Awsm.Xml.child_exn error_response_xml "Error" in
              try
                let error_code =
                  match Awsm.Xml.child_exn error_xml "Code" with
                  | `Data error_code -> error_code
                  | `El (_, children) ->
                    List.map children ~f:(function
                      | `Data s -> s
                      | `El _ -> "")
                    |> Core.String.concat ~sep:""
                in
                return
                  (Error (`AWS (error_of_xml (Core.String.strip error_code) error_xml)))
              with
              | Failure _ -> generic_error ())
            | `El _ -> generic_error ()))
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
                   | `El (((_, "ErrorResponse"), _), _) as error_response_xml ->
                       let error_xml =
                         Awsm.Xml.child_exn error_response_xml "Error" in
                       (try
                          let error_code =
                            match Awsm.Xml.child_exn error_xml "Code" with
                            | `Data error_code -> error_code
                            | `El (_, children) ->
                                (List.map children
                                   ~f:(function | `Data s -> s | `El _ -> ""))
                                  |> (Core.String.concat ~sep:"") in
                          return
                            (Error
                               (`AWS
                                  (error_of_xml (Core.String.strip error_code)
                                     error_xml)))
                        with | Failure _ -> generic_error ())
                   | `El _ -> generic_error ())) in
       match endpoint with
       | Name1 ->
           (match resp with
            | Error err -> handle_error err None
            | Ok resp ->
                (Awsm.Http.Response.body_to_string state resp) >>=
                  ((fun xmls ->
                      let xml = Awsm.Xml.parse_response xmls in
                      return (Ok (ResultModule1.of_xml xml)))))
       | Name2 ->
           (match resp with
            | Error err -> handle_error err None
            | Ok resp ->
                (Awsm.Http.Response.body_to_string state resp) >>=
                  ((fun xmls ->
                      let xml = Awsm.Xml.parse_response xmls in
                      return (Ok (ResultModule2.of_xml xml)))))
       | Name3 -> return (Ok ()) : ((o,
                                      [ `AWS of e
                                      | `Transport of Awsm.Http.Io.Error.call ])
                                      result,
                                     s) Awsm.Http.Monad.app) |}]
;;

let make_structure_for_protocol _service _metadata data =
  [ to_request data; of_response data ]
;;
