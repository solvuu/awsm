open! Core
open! Import

val make : date:string -> service:string -> string
val make_io : [ `Async | `Lwt ] -> date:string -> service:string -> string
val make_cli_async : service:string -> string
