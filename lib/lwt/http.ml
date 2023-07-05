open! Import
open Lwt.Infix

module Io = struct
  include Awsm.Http.Monad.Make (struct
    type +'a t = 'a Lwt.t
  end)

  let monad =
    { Awsm.Http.Monad.bind =
        (fun x f ->
          let open Lwt.Infix in
          inj (prj x >>= fun x -> prj (f x)))
    ; return = (fun x -> inj (Lwt.return x))
    }
  ;;

  let make_stream stream () = inj (Lwt_stream.get stream)

  module Call : sig
    val cohttp_lwt
      :  ?endpoint_url:string
      -> cfg:Awsm.Cfg.t
      -> service:Awsm.Service.t
      -> Awsm.Http.Meth.t
      -> Awsm.Http.Request.t
      -> Uri.t
      -> (t Awsm.Http.Response.t, Awsm.Http.Io.Error.call) result Lwt.t
  end = struct
    let find_xml_redirect_endpoint xml =
      let get x = Awsm.Xml.child_exn xml x |> Awsm.Xml.string_data_exn in
      let code = get "Code" in
      assert (String.equal "PermanentRedirect" code);
      get "Endpoint"
    ;;

    let set_host_headers headers ~host = Cohttp.Header.replace headers "host" host

    let set_host request ~host =
      { request with
        Cohttp.Request.headers =
          request |> Cohttp.Request.headers |> set_host_headers ~host
      }
    ;;

    let rec interpret_response ~limit req_body request (resp, body)
      : (Cohttp.Response.t * Cohttp.Body.t, Awsm.Http.Io.Error.call) result s
      =
      if limit >= 50
      then Lwt.return (Error `Too_many_redirects)
      else (
        match Cohttp.Response.status resp with
        | #Cohttp.Code.success_status -> Lwt.return (Ok (resp, body))
        | #Cohttp.Code.redirection_status ->
          Cohttp.Body.to_string body
          >>= fun body ->
          let xml = Awsm.Xml.parse_response body in
          let host = find_xml_redirect_endpoint xml in
          let new_request = set_host request ~host in
          Cohttp.Client.call
            ~chunked:false
            ~headers:(Cohttp.Request.headers new_request)
            ~body:req_body
            (Cohttp.Request.meth new_request)
            (Cohttp.Request.uri new_request)
          >>= interpret_response ~limit:(succ limit) req_body new_request
        | code ->
          Cohttp.Body.to_string body
          >>= fun body ->
          let x_amzn_error_type =
            let headers = Cohttp.Response.headers resp in
            match Cohttp.Header.get headers "x-amzn-ErrorType" with
            | None -> None
            | Some value -> (
              match String.lsplit2 value ~on:':' with
              | None -> Some value
              | Some (v, _) -> Some v)
          in
          let bad_response =
            { Awsm.Http.Io.Error.code = Cohttp.Code.code_of_status code
            ; body
            ; x_amzn_error_type
            }
          in
          Lwt.return (Error (`Bad_response bad_response)))
    ;;

    let interpret_response = interpret_response ~limit:0

    (** Wrapper around [Cohttp.Client.request] that always uses https.

    @see <https://github.com/mirage/ocaml-cohttp/issues/670> *)
    let cohttp_lwt_client_request request req_body =
      (* 2022-10-24 mbac: the Async version uses [Cohttp.Client.request request], which doesn't
         exist in Lwt version. So we're trying [Cohttp.Client.call] instead. *)
      Cohttp.Client.call
        ~chunked:false
        ~headers:(Cohttp.Request.headers request)
        ~body:(Cohttp.Body.of_string req_body)
        (Cohttp.Request.meth request)
        (Uri.with_scheme (Cohttp.Request.uri request) (Some "https"))
    ;;

    let request_and_follow request req_body =
      cohttp_lwt_client_request request req_body
      >>= interpret_response (Cohttp.Body.of_string req_body) request
    ;;

    let stream_of_body = function
      | `Empty -> fun () -> monad.return None
      | `String x -> fun () -> monad.return (Some x)
      | `Strings l -> fun () -> monad.return (Some (String.concat ~sep:"" l))
      | `Stream s -> make_stream s
    ;;

    let cohttp_lwt ?endpoint_url ~cfg ~service meth request uri =
      let { Awsm.Cfg.region
          ; aws_access_key_id
          ; aws_secret_access_key
          ; aws_session_token
          ; _
          }
        =
        cfg
      in
      let region = Option.value_exn region ~message:"config must set 'region'" in
      let meth = Cohttp.to_meth meth in
      let endpoint =
        match endpoint_url with
        | Some endpoint_url -> Uri.of_string endpoint_url
        | None -> Awsm.Botocore_endpoints.lookup_uri ~region service `HTTPS
      in
      let uri =
        Uri.with_uri ~scheme:(Uri.scheme endpoint) ~host:(Uri.host endpoint) uri
      in
      let host =
        Core.Option.value_exn
          (Uri.host endpoint)
          ~message:
            (sprintf "could not extract 'host' from url %s" (Uri.to_string endpoint))
      in
      let headers =
        let headers = Cohttp.to_headers request in
        Cohttp.Header.add headers "host" host
      in
      let req_body = Awsm.Http.Request.body request in
      let body_length = Int64.of_int (String.length req_body) in
      let payload_hash = Awsm.Auth.payload_hash req_body in
      (*
      eprintf "request: %s\n" (uri |> Uri.to_string);
      eprintf "headers:\n";
      List.iter (headers |> Cohttp.Header.to_list) ~f:(fun (k, v) ->
        eprintf " %s=%s\n" k v);
      eprintf "body: %s\n" req_body;
      eprintf "meth: %s\n" (Cohttp.Code.string_of_method meth);
      eprintf "\n%!";
      *)
      let request =
        Cohttp.Request.make_for_client ~headers ~chunked:false ~body_length meth uri
        |> Awsm.Auth.sign_request
             ~region
             ~service
             ?session_token:aws_session_token
             ?aws_access_key_id
             ?aws_secret_access_key
             ~payload_hash
      in
      request_and_follow request req_body
      >>= function
      | Error _ as err -> Lwt.return err
      | Ok (resp, body) ->
        let version = Cohttp.of_version resp in
        let headers = Cohttp.of_headers resp in
        let status = Cohttp.of_status resp in
        let body = stream_of_body body in
        Lwt.return (Ok (Awsm.Http.Response.make ~version ~headers ~body status))
    ;;
  end

  let make_http http meth request uri = inj (http meth request uri)

  let call ?endpoint_url ~cfg ~service meth request uri =
    make_http (Call.cohttp_lwt ?endpoint_url ~cfg ~service) meth request uri
  ;;
end
