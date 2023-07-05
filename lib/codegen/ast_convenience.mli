(** This module was adapted from ppx_tools_versioned's Ast_convenience_404 in support
   of porting our code from ppx_tools_versioned to ppxlib. *)
open! Core

open! Import

module Label : sig
  type t = Asttypes.arg_label

  type desc = Asttypes.arg_label =
    | Nolabel
    | Labelled of label
    | Optional of label

  val explode : 'a -> 'a
  val nolabel : t
  val labelled : label -> t
  val optional : label -> t
end

val mkloc : 'a -> location -> 'a loc
val mknoloc : 'a -> 'a loc
val default_loc : location ref
val lid : ?loc:location -> label -> longident loc
val evar : ?loc:location -> ?attrs:Ast_helper.attrs -> label -> expression
val may_tuple : ?loc:'a -> (?loc:'a -> ?attrs:'b -> 'c list -> 'c) -> 'c list -> 'c option
val pconstr : ?loc:location -> ?attrs:Ast_helper.attrs -> label -> pattern list -> pattern

val constr
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> label
  -> expression list
  -> expression

val pvar : ?loc:location -> ?attrs:Ast_helper.attrs -> label -> pattern
val str : ?loc:location -> ?attrs:Ast_helper.attrs -> label -> expression
val unit : ?loc:location -> ?attrs:Ast_helper.attrs -> unit -> expression
val tuple : ?loc:location -> ?attrs:Ast_helper.attrs -> expression list -> expression
val ptuple : ?loc:location -> ?attrs:Ast_helper.attrs -> pattern list -> pattern
val nil : ?loc:location -> ?attrs:Ast_helper.attrs -> unit -> expression

val cons
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> expression
  -> expression
  -> expression

val some : expression -> expression
val pair : expression -> expression -> expression
val singleton : expression -> expression
val int : ?loc:location -> ?attrs:Ast_helper.attrs -> int -> expression
val float : ?loc:location -> ?attrs:Ast_helper.attrs -> float -> expression

val app
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> expression
  -> expression list
  -> expression

val app_labels
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> expression
  -> (arg_label * expression) list
  -> expression

val list : ?loc:location -> ?attrs:Ast_helper.attrs -> expression list -> expression
val int32 : ?loc:location -> ?attrs:Ast_helper.attrs -> int32 -> expression
val int64 : ?loc:location -> ?attrs:Ast_helper.attrs -> int64 -> expression

val record
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> ?over:expression
  -> (label * expression) list
  -> expression

val lam
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> ?label:arg_label
  -> ?default:expression
  -> pattern
  -> expression
  -> expression

val pstr : ?loc:location -> ?attrs:Ast_helper.attrs -> label -> pattern

val variant
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> label
  -> expression option
  -> expression

val let_
  :  ?loc:location
  -> ?attrs:Ast_helper.attrs
  -> rec_flag
  -> value_binding list
  -> expression
  -> expression

val http_method : ?loc:location -> Botodata.http_method -> expression

val structure_items_to_module_structure
  :  ?loc:location
  -> label
  -> Astlib.Ast_500.Parsetree.structure
  -> structure_item
