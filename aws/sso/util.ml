(* In sha1_insecure below, [@alert "-crypto"] suppresses the "SHA1 is broken" alert
   from Cryptokit. We know. AWS credential file caching require this for cache lookups still,
   unfortunately. *)

let sha1_insecure s =
  (let h = Cryptokit.Hash.sha1 () in
   let hh = Cryptokit.hash_string h s in
   Cryptokit.transform_string (Cryptokit.Hexa.encode ()) hh) [@alert "-crypto"]
;;

let validate_expiration expiration =
  let expiration =
    expiration
    |> Option.value_exn
         ~message:
           "Sso.get_role_credentials returned roleCredentials with empty expiration??"
    |> Int64.to_float
    |> Time.Span.of_sec
    |> Time.of_span_since_epoch
  in
  let now = Time.now () in
  if Time.( < ) expiration now
  then
    failwithf
      !"Sso.get_role_credentials returned roleCredentials that were already expired! \
        %{Time.to_string_utc} < now (%{Time.to_string_utc})"
      expiration
      now
      ()
;;

let get_cached_sso_token_file_path ~cfg =
  let fn =
    match cfg.Awsm.Cfg.aws_session_token with
    | None | Some "" -> (
      match cfg.Awsm.Cfg.sso_start_url with
      | None | Some "" -> None
      | Some x -> Some x)
    | Some x -> Some x
  in
  match fn with
  | None ->
    failwithf
      "No cached SSO credentials found for URL %s; run `aws sso login`"
      (cfg.Awsm.Cfg.sso_start_url |> Option.value ~default:"<none>")
      ()
  | Some fn ->
    let sha1 = sha1_insecure fn in
    sprintf "%s/.aws/sso/cache/%s.json" (Sys.getenv_exn "HOME") sha1
;;

let get_sso_role_request_and_cfg_exn ~cfg ~cached_sso_token_file jsonstr =
  let json = Awsm.Json.from_string jsonstr in
  let member_string member =
    match Awsm.Json.Util.member_or_null member json with
    | `Null -> failwithf "No '%s' found in %s" member cached_sso_token_file ()
    | `String access_token -> access_token
    | _ ->
      failwithf
        "'%s' from %s has unexpected type; wanted string"
        member
        cached_sso_token_file
        ()
  in
  let request =
    Values.GetRoleCredentialsRequest.make
      ~accountId:
        (cfg.Awsm.Cfg.sso_account_id
        |> Option.value_exn ~message:"No 'sso_account_id' set in credentials")
      ~accessToken:(member_string "accessToken")
      ~roleName:
        (cfg.Awsm.Cfg.sso_role_name
        |> Option.value_exn ~message:"No 'sso_role_name' set in credentials")
      ()
  in
  let sso_region = member_string "region" |> Awsm.Region.of_string in
  let cfg = { cfg with region = Some sso_region } in
  request, cfg
;;

let get_sso_role_request_and_cfg ~cfg ~cached_sso_token_file jsonstr =
  Result.try_with (fun () ->
    get_sso_role_request_and_cfg_exn ~cfg ~cached_sso_token_file jsonstr)
;;

let parse_role_credentials_response_exn = function
  | Ok ({ roleCredentials } : Values.GetRoleCredentialsResponse.t) -> roleCredentials
  | Error (`AWS (`UnauthorizedException (ue : Values.UnauthorizedException.t))) ->
    eprintf "Unauthorized: %s\n" (Option.value ~default:"<no message given>" ue.message);
    eprintf "Maybe you need to re-run `aws sso login`?\n";
    exit 1
  | Error (`AWS err) ->
    failwiths
      ~here:[%here]
      "Sso.get_role_credentials: AWS call"
      err
      Values.GetRoleCredentialsResponse.sexp_of_error
  | Error (`Transport err) ->
    failwiths
      ~here:[%here]
      "Sso.get_role_credentials: transport"
      err
      Awsm.Http.Io.Error.sexp_of_call
;;

let update_cfg_with_role_credentials_exn ~(cfg : Awsm.Cfg.t) role_credentials =
  let ({ accessKeyId; secretAccessKey; sessionToken; expiration }
        : Values.RoleCredentials.t)
    =
    role_credentials
    |> Option.value_exn
         ~message:"Sso.get_role_credentials returned empty roleCredentials??"
  in
  let () = validate_expiration expiration in
  let not_empty s v =
    match v with
    | None | Some "" -> failwithf "Sso.get_role_credentials returned empty %s" s ()
    | Some _ -> v
  in
  { cfg with
    aws_access_key_id = not_empty "accessKeyId" accessKeyId
  ; aws_secret_access_key = not_empty "secretAccessKey" secretAccessKey
  ; aws_session_token = not_empty "sessionToken" sessionToken
  }
;;

let update_cfg_with_role_credentials ~(cfg : Awsm.Cfg.t) role_credentials =
  Result.try_with (fun () -> update_cfg_with_role_credentials_exn ~cfg role_credentials)
;;
