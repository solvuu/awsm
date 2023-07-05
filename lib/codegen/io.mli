open! Core
open! Import

val eval_structure
  :  base_module:string
  -> io_subsystem:[ `Async | `Lwt ]
  -> Endpoint.t list
  -> Parsetree.structure

val eval_signature
  :  protocol:Botodata.protocol
  -> base_module:string
  -> io_subsystem:[ `Async | `Lwt ]
  -> Endpoint.t list
  -> Parsetree.signature
