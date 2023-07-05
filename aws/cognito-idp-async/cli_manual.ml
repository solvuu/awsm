open Awsm_async
open! Import

let cognito_list_user_pools' cfg ?max_results () =
  Util.list_user_pools cfg ?max_results ()
  >>= fun l ->
  let s =
    Util.user_pools_to_string
      (Option.value ~default:[] l.Values.ListUserPoolsResponse.userPools)
  in
  Writer.write (force Writer.stdout) s;
  return ()
;;

(*
  >>= function
  | #Cognito_idp.Api.listUserPools_error -> failwith "user pool error"
  | `Ok l ->
    Cognito_idp.user_pools_to_string (Option.value ~default:[] l.userPools)
    >>| fun s -> Writer.write (force Writer.stdout) s
    *)

let cognito_list_user_pools : Command.t =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Gets a list Cognito User Pools"
    [%map_open
      let log_level = Param.log_level
      and profile = Param.profile
      and max_results = Param.max_results in
      fun () ->
        Log.Global.set_level log_level;
        Cfg.get_exn ?profile ()
        >>= fun cfg -> cognito_list_user_pools' ?max_results cfg ()]
;;

let cognito_idp =
  Command.group
    ~summary:"Cognito Identity Provider Operations"
    [ "list-user-pools", cognito_list_user_pools ]
;;

let main = Command.group ~summary:"AWS client" [ "cognito-idp", cognito_idp ]
