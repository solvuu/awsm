(** Implementation of Secure Remote Password (SRP) authenticate protocol for AWS
    cognito authentication flow. Never directly sends the password over the
    internet, but instead cryptographically strong representation verified on
    both server and client. See -
    https://en.wikipedia.org/wiki/Secure_Remote_Password_protocol -
    https://github.com/capless/warrant for technical details and reference
    implementations. *)

open Awsm
open! Import

(** {2 High level api} *)

module MyCryptokitBignum : sig
  type t
end

(* Necessary parameters to initiate SRP authentication *)
type params_a =
  { k : MyCryptokitBignum.t
  ; a_hex : [ `Encoded_hex of string ]
  ; small_a : MyCryptokitBignum.t
  }

(** Produces necessary parameters to initiate SRP authentication represented in
    strongly encrypted [params_a] record.

    In general, users need not specify any the optional parameters that control
    the cryptographic calculations but serve test purposes and otherwise.

    Raises weakly typed exceptions for any invalid inputs. *)
val ephemeral_a
  :  ?bits:int
  -> ?g:MyCryptokitBignum.t
  -> ?small_a:MyCryptokitBignum.t
  -> ?modulo:MyCryptokitBignum.t
  -> unit
  -> params_a

(** Produces a base64 encoded signature given user [username] on cognito pool
    [user_pool_id] with unencrypted password [password], amazon formatted
    timestamp [timestamp] and cryptographic parameters computed by a previous
    call to [ephemeral_a] in conjunction to values given as a challenge response
    from the server.

    Raises weakly typed exceptions for any invalid inputs. *)
val signature
  :  ?modulo:MyCryptokitBignum.t
  -> ?g:MyCryptokitBignum.t
  -> salt_hex:[ `Encoded_hex of string ]
  -> secret_block_base64:[ `Encoded_base64 of string ]
  -> a_hex:[ `Encoded_hex of string ]
  -> b_hex:[ `Encoded_hex of string ]
  -> small_a:MyCryptokitBignum.t
  -> username:string
  -> user_pool_id:string
  -> k:MyCryptokitBignum.t
  -> password:string
  -> timestamp:string
  -> unit
  -> [ `Encoded_base64 of string ]
