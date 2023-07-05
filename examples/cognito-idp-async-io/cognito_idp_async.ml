open Core
open Awsm_cognito_idp_async.Io
open Awsm_cognito_idp.Values

let http ~cfg = Awsm_async.Http.Io.call ~cfg ~service
let failwithf fmt = Format.kasprintf failwith fmt

let pool_name =
  let open Command.Param in
  anon ("pool_name" %: string)
;;

module List_user_pools = struct
  let run () =
    let open Async.Deferred in
    Awsm_async.Cfg.get_exn ()
    >>= fun cfg ->
    list_user_pools (http ~cfg) (ListUserPoolsRequest.make ~maxResults:10 ())
    >>| function
    | Ok response -> ListUserPoolsResponse.sexp_of_t response |> print_s
    | Error (`Transport _) -> failwithf "list_user_pools: http error" ()
    | Error (`AWS err) ->
      failwithf
        "list_user_pools: %s"
        (err |> ListUserPoolsResponse.sexp_of_error |> Sexp.to_string_hum)
        ()
  ;;

  let param =
    let open Command.Param in
    return run
  ;;

  let command = Async.Command.async ~summary:"List user pools" param
end

module Create_user_pool = struct
  let run poolName () =
    let open Async.Deferred in
    Awsm_async.Cfg.get_exn ()
    >>= fun cfg ->
    create_user_pool (http ~cfg) (CreateUserPoolRequest.make ~poolName ())
    >>| function
    | Ok response -> CreateUserPoolResponse.sexp_of_t response |> print_s
    | Error (`Transport _) -> failwithf "create_user_pool: http error" ()
    | Error (`AWS err) ->
      failwithf
        "create_user_pool: %s"
        (err |> CreateUserPoolResponse.sexp_of_error |> Sexp.to_string_hum)
        ()
  ;;

  let param =
    let open Command.Param in
    return run <*> pool_name
  ;;

  let command = Async.Command.async ~summary:"Create a user pool" param
end

let command =
  Command.group
    ~summary:"Interact with the Cognito IDP API"
    [ "list-user-pools", List_user_pools.command
    ; "create-user-pool", Create_user_pool.command
    ]
;;

let () = Command_unix.run command
