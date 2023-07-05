let idp_call cfg = Awsm_async.Http.Io.call ~cfg ~service:Values.service
let default_max_results = 20

let list_user_pools cfg ?(max_results = default_max_results) () =
  Log.Global.debug "list-user-pools" ~tags:[ "max_results", Int.to_string max_results ];
  let maxResults = Values.PoolQueryLimitType.make max_results in
  match%bind
    Io.list_user_pools (idp_call cfg) (Values.ListUserPoolsRequest.make ~maxResults ())
  with
  | Error _ -> failwithf "idp.list_user_pools" ()
  | Ok x -> return x
;;

let user_pools_to_json t = Values.UserPoolListType.to_json t

let user_pools_to_string t =
  let x = user_pools_to_json t in
  Awsm.Json.to_string x
;;

module Attribute = struct
  module Unsafe = struct
    include Values.AttributeType
    module Name = Values.AttributeNameType
    module Value = Values.AttributeValueType
  end

  module Safe = struct
    include Awsm_cognito_idp.User.Attribute

    let value_string ?here ?message ?error value =
      let v = Option.value_exn ?here ?message ?error value |> Unsafe.Value.to_value in
      match v with
      | `String s -> s
      | _ ->
        failwith
          (match message with
           | None -> "value not a string shape"
           | Some m -> m)
    ;;

    let value_string_opt ?message value =
      let x =
        match value with
        | Some x ->
          let x = Unsafe.Value.to_value x in
          Some x
        | None -> None
      in
      Option.map x ~f:(function
        | `String s -> s
        | _boto_value ->
          failwith
            (match message with
             | None -> "value not a string shape"
             | Some m -> m))
    ;;

    let of_unsafe { Unsafe.name; value } =
      match Unsafe.Name.to_value name with
      | `String "email" ->
        value_string ?here:None ?error:None ~message:"email must be a string shape" value
        |> fun x -> `Email x
      | `String "name" ->
        value_string ?here:None ?error:None ~message:"name must be a string shape" value
        |> fun x -> `Name x
      | `String "preferred_user_name" ->
        value_string_opt ~message:"prefered user name must be a string shape" value
        |> fun x -> `Preferred_user_name x
      | `String s -> (
        match String.split s ~on:':' with
        | "custom" :: rest ->
          let name = String.concat ~sep:":" rest in
          value_string_opt value |> fun value -> `Custom { name; value }
        | _ -> value_string_opt value |> fun value -> `Unknown { name = s; value })
      | _ -> failwith "not a valid shape for attribute key"
    ;;
  end
end

module User = Awsm_cognito_idp.User

type get_user_error =
  [ `AWS of Values.GetUserResponse.error
  | `Transport of Awsm.Http.Io.Error.call
  ]
[@@deriving sexp]

exception
  (* Convenience exception for [get_user] AWS calls which wraps the many error
     condition variants *)
    Get_user_error of
    { message : string option
    ; cause : get_user_error
    }

let get_user_failwith ?message cause = raise (Get_user_error { message; cause })

let get_user ?retry_delay ?retry_cnt cfg ~access_token () =
  Awsm_async.Import.with_retries ?retry_delay ?retry_cnt
  @@ fun () ->
  let accessToken = Values.TokenModelType.make access_token in
  let request = Values.GetUserRequest.make ~accessToken () in
  match%map Io.get_user (idp_call cfg) request with
  | Error e -> Error e
  | Ok
      ({ Values.GetUserResponse.username
       ; userAttributes
       ; mFAOptions = _
       ; preferredMfaSetting = _
       ; userMFASettingList = _
       } as response) ->
    let x = Values.GetUserResponse.to_json response in
    let resp_str = Awsm.Json.to_string x in
    Log.Global.debug "response body is %s" resp_str;
    let username =
      match Values.UsernameType.to_value username with
      | `String username -> username
      | _ -> failwith "username shape not a string"
    in
    List.map ~f:Attribute.Safe.of_unsafe userAttributes
    |> fun attributes -> Ok User.{ username; attributes; access_token }
;;

let admin_get_user ?retry_delay ?retry_cnt cfg ~user_pool_id ~username () =
  Awsm_async.Import.with_retries ?retry_delay ?retry_cnt
  @@ fun () ->
  Io.admin_get_user
    (idp_call cfg)
    (Values.AdminGetUserRequest.make
       ~userPoolId:(Values.UserPoolIdType.make user_pool_id)
       ~username:(Values.UsernameType.make username)
       ())
  >>= fun response ->
  match response with
  | Error response -> return (Error response)
  | Ok
      ({ Values.AdminGetUserResponse.username
       ; userAttributes
       ; mFAOptions = _
       ; userCreateDate = _
       ; userLastModifiedDate = _
       ; enabled = _
       ; userStatus = _
       ; preferredMfaSetting = _
       ; userMFASettingList = _
       } as response) ->
    let userAttributes = Option.value userAttributes ~default:[] in
    let x = Values.AdminGetUserResponse.to_json response in
    let resp_str = Awsm.Json.to_string x in
    Log.Global.debug "response body is %s" resp_str;
    let username =
      match Values.UsernameType.to_value username with
      | `String username -> username
      | _ -> failwith "username shape not a string"
    in
    List.map ~f:Attribute.Safe.of_unsafe userAttributes
    |> fun attributes -> return (Ok User.{ username; attributes; access_token = "" })
;;
