open! Core
open! Import

(* This is a copy/paste of Yojson.Safe.t with `Tuple and `Variant removed.
   We mant the safe `Intlit loading without the other OCaml-specific extensions. *)
type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Intlit of string
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list
  ]
[@@deriving sexp]

exception Json_error of string

val from_string : string -> t
val to_string : t -> string

module Util : sig
  exception Type_error of (string * t)

  (* [member_or_null k assoc] finds the value for key [k] in a JSON assoc or
     evaluates to `Null if the member is not found. *)
  val member_or_null : string -> t -> t
  val field_map_exn : t -> string -> (t -> 'a) -> 'a
  val field_map : t -> string -> (t -> 'a) -> 'a option
end

(*
type ('a, 's) stream = unit -> ('a option, 's) Monad.app

type error =
  [ Jsonm.error
  | `Unexpected_end_of_input
  | `Unexpected_end_of_object
  | `Unexpected_name of string
  | `Unexpected_lexeme of Jsonm.lexeme
  ]

val pp_error : Format.formatter -> error -> unit

val stream_to_json
  :  's Monad.t
  -> (string, 's) stream
  -> (value -> (('a, error) result, 's) Monad.app)
  -> (('a, error) result, 's) Monad.app
*)
