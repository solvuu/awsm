module Statement = struct
  type effect =
    | Accept
    | Deny
  [@@deriving sexp]

  type action = string list [@@deriving sexp]
  type resource = string list [@@deriving sexp]

  let effect_to_string = function
    | Accept -> "Allow"
    | Deny -> "Deny"
  ;;

  let default_effect = Accept
  let default_sid = "solvuu"

  (* TODO what is this ? *)
  type t =
    { sid : string
    ; effect : effect
    ; action : action
    ; resource : resource
    }
  [@@deriving sexp]

  let create ?(effect = default_effect) ?(sid = default_sid) ~action ~resource () =
    { sid; effect; action; resource }
  ;;

  (* FIXME: Below comment says we're avoiding a dependency on yojson. However, the mli
     refers to Yojson so we're not.

     Manually compile json string to temporarily avoid yojson dependency *)
  let to_json t =
    `Assoc
      [ "Sid", `String t.sid
      ; "Effect", `String (effect_to_string t.effect)
      ; "Action", `List (List.map ~f:(fun s -> `String s) t.action)
      ; "Resource", `List (List.map ~f:(fun s -> `String s) t.resource)
      ]
  ;;

  let to_json_all l = `List (List.map ~f:to_json l)
end

module Policy = struct
  type t =
    { version : string
    ; statement : Statement.t list
    }
  [@@deriving sexp]

  let to_json t =
    `Assoc
      [ "Version", `String t.version; "Statement", Statement.to_json_all t.statement ]
  ;;

  let create ?(version = "2012-10-17") statement =
    let t = { version; statement } in
    Log.Global.debug "policy as json: %s" (to_json t |> Awsm.Json.to_string);
    t
  ;;
end

let assume_role
  ?(policy : Policy.t option)
  ?retry_delay
  ?retry_cnt
  ?duration_sec
  ~session_name
  ~role
  cfg
  =
  Awsm_async.Import.with_retries ?retry_delay ?retry_cnt (fun () ->
    match%bind
      Io.assume_role
        (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
        (Values.AssumeRoleRequest.make
           ~roleArn:(Values.ArnType.make role)
           ~roleSessionName:(Values.RoleSessionNameType.make session_name)
           ?durationSeconds:
             (Option.map duration_sec ~f:Values.RoleDurationSecondsType.make)
           ?policy:
             (Option.map
                ~f:(fun x ->
                  Policy.to_json x
                  |> Awsm.Json.to_string
                  |> Values.SessionPolicyDocumentType.make)
                policy)
           ())
    with
    | Ok v -> return v
    | Error _ -> failwithf "sts.assume_role" ())
;;

let assume_role_with_saml
  ?(policy : Policy.t option)
  ?retry_delay
  ?retry_cnt
  ?duration_sec
  ~principal_arn
  ~saml_assertion
  ~role
  cfg
  =
  Awsm_async.Import.with_retries ?retry_delay ?retry_cnt (fun () ->
    match%bind
      Io.assume_role_with_s_a_m_l
        (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
        (Values.AssumeRoleWithSAMLRequest.make
           ~principalArn:principal_arn
           ~sAMLAssertion:saml_assertion
           ~roleArn:(Values.ArnType.make role)
           ?durationSeconds:
             (Option.map duration_sec ~f:Values.RoleDurationSecondsType.make)
           ?policy:
             (Option.map
                ~f:(fun x ->
                  Policy.to_json x
                  |> Awsm.Json.to_string
                  |> Values.SessionPolicyDocumentType.make)
                policy)
           ())
    with
    | Ok v -> return v
    | Error _ -> failwithf "sts.assume_role" ())
;;
