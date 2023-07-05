open! Core
open! Import

(** An abstract representation of a URL query parameter. *)
type t [@@deriving sexp_of]

(** {2 Constructors} *)

(** Build from fields.

    @param name The (http) name of the parameter.

    @param shape The boto shape name in the operation.

    @param is_required Is this required or optional?
     *)
val create : name:string -> field_name:string -> shape:string -> is_required:bool -> t

(** Build from a parsed {!Botodata.operation}. *)
val of_botodata
  :  Botodata.operation
  -> shapes:(string, Botodata.shape) List.Assoc.t
  -> t list

(** {2 Accessors } *)

val is_required : t -> bool

(** {2 Helpers } *)

(** The HTTP parameter name (the locationName). *)
val param_name : t -> string

(** The field name in the name in the ocaml record type. *)
val field_name : t -> string

(** {2 Code generation} *)

(** Emit a converter.

    If the corresponding [shape] is [Mod], emit [Mod.to_header]. *)
val convert : t -> Parsetree.expression
