open! Core
open! Import

(** A parser returning a value of type ['a].

    It can act on a JSON value using the [run] function. *)
type 'a t

(** Run the parser against a JSON value. The [Error] case contains information
    about where an error occured in the JSON document. *)
val run : 'a t -> Json.t -> ('a, string) Result.t

val run_exn : 'a t -> Json.t -> 'a

(** Low-level way to define a parser. Prefer combining other functions from this
    module, as it will return better error messages.

    When returning an error, only include the error message: the location data
    will be added automatically by this module. *)
val parse_with : (Json.t -> ('a, string) Result.t) -> 'a t

(** Map over a parser. *)
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

(** Map over a parser, with a function that can return an error. *)
val map_result : 'a t -> f:('a -> ('b, string) result) -> 'b t

(** Convert a [string]. *)
val string : string t

(** Convert an [int]. Fails if a number is not exactly an integer. *)
val int : int t

val float : float t

(** Convert an [int64]. Fails if a number is not exactly an [int64]. *)
val int64 : int64 t

(** Convert a [bool]. *)
val bool : bool t

(** Convert a list of values. *)
val list : 'a t -> 'a list t

(** Convert an homogeneous dictionary. It is an object where all values have the
    same type. For example, [{"x": 1, "y": 2, "z": 3}] can be parsed using [dict
    int] to [[("x", 1); ("y", 2); ("z", 3)]]. *)
val dict : 'a t -> (string * 'a) list t

(** An abstract value representing values in a JSON record. To parse such a
    record, use [record]. *)
type 'a record

(** Create a record that contains a static value. *)
val return : 'a -> 'a record

(** Applicative interface for [record]. The [ppx_let] syntax extension uses this
    to translate [let%map x1 = v1 and x2 = v2 in e]. *)
module Let_syntax : sig
  val map : 'a record -> f:('a -> 'b) -> 'b record
  val both : 'a record -> 'b record -> ('a * 'b) record
end

(** A required field. If not present, will return an error. *)
val field : string -> 'a t -> 'a record

(** An optional field. If not present, will return [None]. *)
val field_opt : string -> 'a t -> 'a option record

(** Like [field_opt], but will return [default] if the field is not present. *)
val field_or : string -> 'a t -> default:'a -> 'a record

(** Ignore this field. This completely ignores the presence, type and value of
    this field. It is usually better to parse the field with its correct type,
    and ignore it in the calling code. *)
val field_ignored : string -> unit record

(** Parse a JSON record. In addition to returning the correct data, it will also
    check that all fields in the record have been parsed explicitly. *)
val record : 'a record -> 'a t

(** If the JSON value being parsed is a record, parse it (and return a
    singleton). If it is a list, parse a list of records. *)
val record_or_list_of : 'a record -> 'a list t

(** Support for type/value objects. [field_based key get_parser] will parse the
    current JSON value as an object, inspect the string value at the given
    [key], and pass it to [get_parser]. The return value of this function
    determines how the rest of the record will be parsed. *)
val field_based : string -> (string -> 'a record option) -> 'a t

(** Parse using a different function depending on whether a key is present. *)
val if_field_present : string -> then_:'a record -> else_:'a record -> 'a record
