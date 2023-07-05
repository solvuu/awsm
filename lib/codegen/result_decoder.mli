open! Core
open! Import

(** How to decode a result body for the rest_{json,xml} protocol. *)
type t =
  | Xml (* Use [Mod.of_xml] *)
  | Json (* Use [Mod.of_json] *)
  (* Use [Mod.of_header_and_body]. Parameter is the payload module - if
       present, [Payload.of_string] will be used. *)
  | Of_header_and_body of string option
[@@deriving sexp_of]

(** Determine the decoder from a parsed {!Botodata.operation}. *)
val of_botodata_xml
  :  Botodata.operation
  -> shapes:(string, Botodata.shape) List.Assoc.t
  -> t option

val of_botodata_json
  :  Botodata.operation
  -> shapes:(string, Botodata.shape) List.Assoc.t
  -> t option
