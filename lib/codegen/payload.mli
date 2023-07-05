open! Core
open! Import

(** The abstract representation of how to interact with a HTTP body. *)
type t [@@deriving sexp_of]

(** {2 Constructors} *)

(** Build from fields.

    @param is_blob Can this parameter be sent directly (or does it need to be
    converted through [to_header]).

    @param payload_module Where [to_header] and [to_value] functions will be
    found.

    @param field_name Where the payload can be found in the request record.

    @param is_required Is this payload required or optional? *)
val create
  :  is_blob:bool
  -> payload_module:string
  -> field_name:string
  -> is_required:bool
  -> t

(** Build from a parsed {!Botodata.operation}. *)
val of_botodata
  :  Botodata.operation
  -> shapes:(string, Botodata.shape) List.Assoc.t
  -> t option

(** {2 Code generation} *)

(** Generate the expression that will be used for rest_json protocols. *)
val convert_rest_json
  :  t
  -> service:Botodata.service
  -> op:Botodata.operation option
  -> endpoint_name:string
  -> Parsetree.expression
  -> Parsetree.expression

(** Generate the expression that will be used for rest_xml protocols. *)
val convert_rest_xml
  :  t
  -> endpoint_name:string
  -> Parsetree.expression
  -> Parsetree.expression
