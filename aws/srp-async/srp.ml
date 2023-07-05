module Cognito_idp = struct
  module Values = Awsm_cognito_idp_async.Values
  module Io = Awsm_cognito_idp_async.Io

  let call = Awsm_async.Http.Io.call ~service:Values.service

  include Awsm_cognito_idp_async.Util
end

let amzdate (timestamp : Time.t) : string =
  Time_unix.format timestamp ~zone:Time.Zone.utc "%a %b %-d %H:%M:%S UTC %Y"
;;

(*let authFlow = Cognito_idp.Api.AuthFlowType.USER_SRP_AUTH*)
let authFlow = Cognito_idp.Values.AuthFlowType.USER_SRP_AUTH

(* TODO move to test module let default_clientId =
   Cognito_idp.Api.ClientIdType.make "nvgta6tlkbnt9s73uasceds5"

   TODO Move to test module) let default_user_pool_id = "us-east-1_2hqFUKfFd" *)

let challenge_param_exn key params =
  List.find_map_exn ~f:(fun (k, v) -> Option.some_if (String.equal key k) (k, v)) params
;;

let authenticate ~user_pool_id ~client_id ~username ~password =
  let { Awsm_srp.k; a_hex = `Encoded_hex srp_a as a_hex; small_a } =
    Awsm_srp.ephemeral_a ()
  in
  let authParameters =
    Cognito_idp.Values.AuthParametersType.make [ "USERNAME", username; "SRP_A", srp_a ]
  in
  let clientId = Cognito_idp.Values.ClientIdType.make client_id in
  let initiate_auth_request =
    Cognito_idp.Values.InitiateAuthRequest.make ~authParameters ~authFlow ~clientId ()
  in
  let cfg : Awsm.Cfg.t =
    { Awsm.Cfg.empty with
      region = Some Awsm.Region.us_east_1
    ; aws_access_key_id = None
    ; output = Some "json"
    ; aws_session_token = None
    ; aws_secret_access_key = None
    }
  in
  Cognito_idp.Io.initiate_auth (Cognito_idp.call ~cfg) initiate_auth_request
  >>= fun resp ->
  match resp with
  | Error err -> return (Error (`Initiate_auth err))
  | Ok
      { Cognito_idp.Values.InitiateAuthResponse.challengeParameters
      ; challengeName
      ; authenticationResult = _
      ; session = _
      } -> (
    let challengeName =
      Option.value_exn
        ~message:"no challenge name in initiate authorization response."
        challengeName
    in
    let challenge_params =
      Option.value_exn
        ~message:"no challenge parameters in initiate authorization response."
        challengeParameters
    in
    let salt_key, salt_hex = challenge_param_exn "SALT" challenge_params in
    let srp_b_key, srp_b_hex = challenge_param_exn "SRP_B" challenge_params in
    let user_id_for_srp_key, user_id_for_srp =
      challenge_param_exn "USER_ID_FOR_SRP" challenge_params
    in
    let secret_block_key, secret_block_bytes =
      challenge_param_exn "SECRET_BLOCK" challenge_params
    in
    let secret_block_base64 = `Encoded_base64 secret_block_bytes in
    let timestamp = amzdate (Time.now ()) in
    Log.Global.debug
      ~tags:
        [ "TIMESTAMP", timestamp
        ; salt_key, salt_hex
        ; srp_b_key, srp_b_hex
        ; user_id_for_srp_key, user_id_for_srp
        ; secret_block_key, secret_block_bytes
        ]
      "responding to auth challenge";
    let (`Encoded_base64 signature) =
      Awsm_srp.signature
        ~secret_block_base64
        ~k
        ~small_a
        ~salt_hex:(`Encoded_hex salt_hex)
        ~a_hex
        ~b_hex:(`Encoded_hex srp_b_hex)
        ~user_pool_id
        ~username:user_id_for_srp
        ~password
        ~timestamp
        ()
    in
    let challengeResponses =
      Cognito_idp.Values.ChallengeResponsesType.make
        [ "TIMESTAMP", timestamp
        ; "USERNAME", user_id_for_srp
        ; "PASSWORD_CLAIM_SECRET_BLOCK", secret_block_bytes
        ; "PASSWORD_CLAIM_SIGNATURE", signature
        ]
    in
    let challenge_request =
      Cognito_idp.Values.RespondToAuthChallengeRequest.make
        ~clientId
        ~challengeName
        ~challengeResponses
        ()
    in
    Cognito_idp.Io.respond_to_auth_challenge (Cognito_idp.call ~cfg) challenge_request
    >>= fun resp ->
    match resp with
    | Error err -> return (Error (`Respond_to_auth_challenge err))
    | Ok resp -> return (Ok resp))
;;
