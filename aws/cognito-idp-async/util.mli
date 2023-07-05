(** A high level Cognito IDP API. Simplifies some of the complexity of the
    native AWS types and provides a more natural ocaml style interface to some
    commonly used AWS functions which are otherhwise just code generated
    directly from the boto service files. *)

open Awsm_async
open! Import

(* TODO: these functions used to return error types when they were functorized, but the port
    to lightweight doesn't surface them yet (they just raise). This should be fairly easy to
    refactor after we add more robust error support to lightweight code gen. *)

(* A high level version of a cognito user which reduces error prone raw access
   to the user attribute list *)
module User : module type of Awsm_cognito_idp.User

(** Produces [`Ok response] containing the user pools, or a
    [listUserPools_error] upon failure. *)
val list_user_pools
  :  Awsm.Cfg.t
  -> ?max_results:int
  -> unit
  -> Values.ListUserPoolsResponse.t Deferred.t

(*
  -> [ `Ok of Api.ListUserPoolsResponse.t | Api.listUserPools_error ] Deferred.t
  *)

(** Produces a json string representation give an AWS user pools type. *)
val user_pools_to_string : Values.UserPoolListType.t -> string

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

(** Convenience function to wrap a [getUser_error] into an exception type a
    raise it. An optional message can be added to the exception to provide
    additional context *)
val get_user_failwith : ?message:string -> get_user_error -> 'a

(** Produces [`Ok user] given an [access_token] representing a valid,
    authenticated cognito user, or a [getUser_error] upon failure. *)
val get_user
  :  ?retry_delay:Time.Span.t
  -> ?retry_cnt:int
  -> Awsm.Cfg.t
  -> access_token:Base.string
  -> unit
  -> (User.t, get_user_error) result Deferred.t

(** Produces [`Ok user] given a [user_pool_id] and [username], authenticated
    cognito user, or a [getUser_error] upon failure. *)
val admin_get_user
  :  ?retry_delay:Time.Span.t
  -> ?retry_cnt:int
  -> Awsm.Cfg.t
  -> user_pool_id:string
  -> username:string
  -> unit
  -> ( User.t
     , [ `AWS of Values.AdminGetUserResponse.error
       | `Transport of Awsm.Http.Io.Error.call
       ] )
     result
     Deferred.t
