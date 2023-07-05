open! Core
open! Import

let eval_structure ~base_module ~io_subsystem operations =
  let loc = !Ast_helper.default_loc in
  let make binding ctor =
    let p = Ast_convenience.pvar binding in
    let c = Printf.ksprintf Ast_convenience.evar "Endpoints.%s" ctor in
    [%stri let [%p p] = fun http -> eval http [%e c]]
  in
  let loc = !Ast_helper.default_loc in
  let open_ =
    let lid = Ast_convenience.lid base_module in
    let s = Ast_helper.Opn.mk lid in
    (* 2022-05-14 mbac: this got more ugly when porting from ppx_tools_versioned to
       ppxlib. It *seems* not quite right. We should be using the lid after
       creating it from base_module, but instead using base_module again. *)
    let longident_loc txt loc = { txt = Longident.Lident txt; loc } in
    Ast_helper.Str.open_
      { popen_expr =
          { pmod_desc = Pmod_ident (longident_loc base_module loc)
          ; pmod_loc = s.popen_loc
          ; pmod_attributes = []
          }
      ; popen_override = s.popen_override
      ; popen_loc = s.popen_loc
      ; popen_attributes = s.popen_attributes
      }
  in
  let preamble =
    match io_subsystem with
    | `Lwt ->
      [%str
        [%%i open_]

        let eval http endpoint input =
          let open Awsm_lwt.Http.Io in
          let meth = Endpoints.method_of_endpoint endpoint in
          let uri = Endpoints.uri_of_endpoint endpoint input in
          let fiber =
            let ( >>= ) = monad.bind in
            let _return = monad.return in
            http meth (Endpoints.to_request endpoint input) uri
            >>= fun resp_result -> Endpoints.of_response monad endpoint resp_result
          in
          prj fiber
        ;;]
    | `Async ->
      [%str
        [%%i open_]

        let eval http endpoint input =
          let open Awsm_async.Http.Io in
          let meth = Endpoints.method_of_endpoint endpoint in
          let uri = Endpoints.uri_of_endpoint endpoint input in
          let fiber =
            let ( >>= ) = monad.bind in
            let _return = monad.return in
            http meth (Endpoints.to_request endpoint input) uri
            >>= fun resp_result -> Endpoints.of_response monad endpoint resp_result
          in
          prj fiber
        ;;]
  in
  preamble
  @ List.map
      ~f:(fun e ->
        let s = Endpoint.name e in
        make (Util.camel_to_snake_case s) s)
      operations
;;

let%expect_test "eval_structure_async" =
  let data =
    [ Endpoint.create_test "AbortMultipartUpload"
    ; Endpoint.create_test "CompleteMultipartUpload"
    ]
  in
  eval_structure ~io_subsystem:`Async ~base_module:"Base_module" data
  |> Util.structure_to_string
  |> printf "%s%!";
  [%expect
    {|
    open Base_module
    let eval http endpoint input =
      let open Awsm_async.Http.Io in
        let meth = Endpoints.method_of_endpoint endpoint in
        let uri = Endpoints.uri_of_endpoint endpoint input in
        let fiber =
          let (>>=) = monad.bind in
          let _return = monad.return in
          (http meth (Endpoints.to_request endpoint input) uri) >>=
            (fun resp_result -> Endpoints.of_response monad endpoint resp_result) in
        prj fiber
    let abort_multipart_upload http = eval http Endpoints.AbortMultipartUpload
    let complete_multipart_upload http =
      eval http Endpoints.CompleteMultipartUpload |}]
;;

let%expect_test "eval_structure_lwt" =
  let data =
    [ Endpoint.create_test "AbortMultipartUpload"
    ; Endpoint.create_test "CompleteMultipartUpload"
    ]
  in
  eval_structure ~io_subsystem:`Lwt ~base_module:"Base_module" data
  |> Util.structure_to_string
  |> printf "%s%!";
  [%expect
    {|
    open Base_module
    let eval http endpoint input =
      let open Awsm_lwt.Http.Io in
        let meth = Endpoints.method_of_endpoint endpoint in
        let uri = Endpoints.uri_of_endpoint endpoint input in
        let fiber =
          let (>>=) = monad.bind in
          let _return = monad.return in
          (http meth (Endpoints.to_request endpoint input) uri) >>=
            (fun resp_result -> Endpoints.of_response monad endpoint resp_result) in
        prj fiber
    let abort_multipart_upload http = eval http Endpoints.AbortMultipartUpload
    let complete_multipart_upload http =
      eval http Endpoints.CompleteMultipartUpload |}]
;;

let eval_signature ~protocol ~base_module ~io_subsystem endpoints =
  let loc = !Ast_helper.default_loc in
  let open_ =
    base_module
    |> Printf.ksprintf Ast_convenience.lid "%s.Values"
    |> Ast_helper.Opn.mk
    |> Ast_helper.Sig.open_
  in
  [%sig: [%%i open_]]
  @ List.map endpoints ~f:(fun e ->
      let name = Endpoint.name e |> Util.camel_to_snake_case |> Ast_convenience.mknoloc in
      let call_type =
        let io_arg =
          match io_subsystem with
          | `Lwt -> [%type: Awsm_lwt.Http.Io.t]
          | `Async -> [%type: Awsm_async.Http.Io.t]
        in
        [%type: ([%t io_arg], Awsm.Http.Io.Error.call) Awsm.Http.Call.t]
      in
      let request_type = Endpoint.request_type e in
      let result_type =
        let ok_arg = Endpoint.result_ok_type e in
        let error_arg =
          let aws_error =
            match protocol with
            | `ec2 -> [%type: Values.Ec2_error.t]
            | `json | `query | `rest_xml | `rest_json -> Endpoint.result_error_type e
          in
          [%type: [ `AWS of [%t aws_error] | `Transport of Awsm.Http.Io.Error.call ]]
        in
        match io_subsystem with
        | `Async -> [%type: ([%t ok_arg], [%t error_arg]) Result.t Async.Deferred.t]
        | `Lwt -> [%type: ([%t ok_arg], [%t error_arg]) Result.t Lwt.t]
      in
      Ast_helper.Sig.value
        (Ast_helper.Val.mk
           name
           [%type: [%t call_type] -> [%t request_type] -> [%t result_type]]))
;;

let%expect_test "eval_signature_async" =
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
  eval_signature ~protocol:`json ~base_module:"Base_module" ~io_subsystem:`Async data
  |> Util.signature_to_string
  |> print_endline;
  [%expect
    {|
    open Base_module.Values
    val input_and_output :
      (Awsm_async.Http.Io.t, Awsm.Http.Io.Error.call) Awsm.Http.Call.t ->
        Input.t ->
          (Output.t,
            [ `AWS of Output.error  | `Transport of Awsm.Http.Io.Error.call ])
            Result.t Async.Deferred.t
    val only_input :
      (Awsm_async.Http.Io.t, Awsm.Http.Io.Error.call) Awsm.Http.Call.t ->
        Input.t ->
          (unit, [ `AWS of unit  | `Transport of Awsm.Http.Io.Error.call ])
            Result.t Async.Deferred.t
    val only_output :
      (Awsm_async.Http.Io.t, Awsm.Http.Io.Error.call) Awsm.Http.Call.t ->
        unit ->
          (Output.t,
            [ `AWS of Output.error  | `Transport of Awsm.Http.Io.Error.call ])
            Result.t Async.Deferred.t
    val no_input_and_no_output :
      (Awsm_async.Http.Io.t, Awsm.Http.Io.Error.call) Awsm.Http.Call.t ->
        unit ->
          (unit, [ `AWS of unit  | `Transport of Awsm.Http.Io.Error.call ])
            Result.t Async.Deferred.t |}]
;;
