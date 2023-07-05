(** Cognito Secure Remote Password (SRP) protocol implementation. Extends the
    unix version with async support and direct cognito integration via usage of
    the [async] [cognito_idp] module. *)

open Awsm_async
open! Import
(*
module Cognito_idp = Awsm_cognito_idp_async.Cognito_idp
*)

(** Authenticates a user with name [username] and [password] belonging to user
    pool [user_pool_id] with client id [client_id] using the AWS Cognito Secure
    Remote Password (SRP) protocol.

    Upon successful authentication an authentication token is produced along
    with other session information in the [`Ok ..] condition.

    Otherwise, a specific error variant indicates the authentication issue. *)
val authenticate
  :  user_pool_id:string
  -> client_id:string
  -> username:string
  -> password:string
  -> ( Awsm_cognito_idp_async.Values.RespondToAuthChallengeResponse.t
     , [ `Initiate_auth of
         [ `AWS of Awsm_cognito_idp_async.Values.InitiateAuthResponse.error
         | `Transport of Awsm.Http.Io.Error.call
         ]
       | `Respond_to_auth_challenge of
         [ `AWS of Awsm_cognito_idp_async.Values.RespondToAuthChallengeResponse.error
         | `Transport of Awsm.Http.Io.Error.call
         ]
       ] )
     Result.t
     Deferred.t
