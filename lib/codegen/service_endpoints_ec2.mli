open! Core
open! Import

(** Generate structure items specific to the JSON protocol. *)
val make_structure_for_protocol
  :  Botodata.service
  -> Botodata.metadata
  -> Endpoint.t list
  -> Parsetree.structure
