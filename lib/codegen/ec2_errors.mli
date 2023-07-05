open! Core
open! Import

type t =
  { name : label
  ; description : label
  }

module Common_client_errors : sig
  val enumerate : unit -> t list
end

module Client_errors_for_specific_actions : sig
  val enumerate : unit -> t list
end

module Server_errors : sig
  val enumerate : unit -> t list
end

val enumerate_all : unit -> t list
