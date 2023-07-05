open! Core
open! Import

val to_request : Endpoint.t list -> Parsetree.structure_item

val make_error_expression
  :  loc:Location.t
  -> label:string
  -> Endpoint.t
  -> Parsetree.expression
