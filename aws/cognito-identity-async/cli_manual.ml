let cognito_list_identity_pools' cfg ?max_results () =
  Cognito_identity.list_identity_pools cfg ?max_results ()
  >>= fun l ->
  let s = Cognito_identity.identity_pools_to_string l in
  Writer.write (force Writer.stdout) s;
  return ()
;;

let cognito_list_identity_pools : Command.t =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Gets a list Cognito Identity Pools"
    [%map_open
      let log_level = Awsm_async.Param.log_level
      and profile = Awsm_async.Param.profile
      and max_results = Awsm_async.Param.max_results in
      fun () ->
        Log.Global.set_level log_level;
        Awsm_async.Cfg.get_exn ?profile ()
        >>= fun cfg -> cognito_list_identity_pools' cfg ?max_results ()]
;;

let cognito_identity =
  Command.group
    ~summary:"Cognito Identity Operations"
    [ "list-identity-pools", cognito_list_identity_pools ]
;;

let main = Command.group ~summary:"AWS client" [ "cognito-identity", cognito_identity ]
