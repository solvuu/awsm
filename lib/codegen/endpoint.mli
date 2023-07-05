open! Core
open! Import

(* The abstract representation of an API endpoint. This is an higher level
    abstraction that the raw [Botodata] types. *)
type t [@@deriving sexp_of]

(** {2 Constructors} *)

(* Build a value from all fields. *)
val create
  :  name:string
  -> op:Botodata.operation option
  -> request_module:string option
  -> result_module:string option
  -> meth:Botodata.http_method
  -> request_uri:Botodata.requestUri
  -> query_params:Query_param.t list
  -> payload:Payload.t option
  -> result_decoder:Result_decoder.t option
  -> t

(* Factory function for use in tests. It has unspecified defaults for most
    fields. *)
val create_test
  :  ?op:Botodata.operation option
  -> ?request_module:string option
  -> ?result_module:string option
  -> ?meth:Botodata.http_method
  -> ?request_uri:Botodata.requestUri
  -> ?query_params:Query_param.t list
  -> ?payload:Payload.t option
  -> ?result_decoder:Result_decoder.t option
  -> string
  -> t

(* Build a value from a parsed {!Botodata.operation}. *)
val of_botodata : Botodata.operation -> service:Botodata.service -> t

(** {2 Accessors} *)

val name : t -> string
val op : t -> Botodata.operation option
val meth : t -> Botodata.http_method
val request_uri : t -> Botodata.requestUri

(* Query params in the request URL. See {!Query_param.t}. *)
val query_params : t -> Query_param.t list

(* The HTTP payload (body). See {!Payload.t}. *)
val payload : t -> Payload.t option

(* How to decode the response body. See {!Result_decoder.t}. *)
val result_decoder : t -> Result_decoder.t option

(* {2 Helpers} *)

(* The type to pass as request, e.g. [CreateThingyRequest.t]. *)
val request_type : t -> Parsetree.core_type

(* The type to that the operation will return, e.g. [ListThingiesResult.t]. *)
val result_ok_type : t -> Parsetree.core_type

(* The [result_ok_type] above is the Ok part of the result. The Error part is [result_error_type]. *)
val result_error_type : t -> Parsetree.core_type

(* [in_request_module ep name] is an expression representing value [name] in
    the request module.

    For example, if [request_type t] is [CreateThingyRequest.t],
    [in_request_module t "to_json"] is the expression
    [CreateThingyRequest.to_json]. *)
val in_request_module : t -> string -> Parsetree.expression option
val request_module : t -> string option

(* [in_result_module ep name] is an expression representing value [name] in the
    result module.

    For example, if [result_ok_type t] is [ListThingiesResult.t], [in_result_module
    t "of_xml"] is the expression [ListThingiesResult.of_xml]. *)
val in_result_module : t -> string -> Parsetree.expression option
val result_module : t -> string option

(* A helper to write [match] or [function] expressions that iterate on a whole
    set of endpoints. Each branch will look like [name ep -> f ep]. *)
val cases : f:(t -> Parsetree.expression) -> t list -> Parsetree.case list
