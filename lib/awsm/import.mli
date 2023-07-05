(** Extension of Core library. *)

module String : sig
  include module type of String

  val matches : string -> pat:string -> bool
end

(** Utility functions for writing tests. *)
module Test : sig
  (** [pass msg alist] prints a message to stdout indicating that a test passed
        by printing "OK". The [msg] should state what the test was, and the
        association list [alist] should provide the names and values of items
        needed to understand the test.

        It is recommended that [msg] be a constant and not depend on any values
        specific to the test, which should instead be provided in [alist]. This
        makes it easier to process tests that need to be robust to different
        environments. This may not be relevant for unit tests which are likely
        independent of any environment or context, but following this guideline
        nonetheless leads to consistent expect test formatting. *)
  val pass : string -> (string * string) list -> unit

  (** [fail msg alist] is similar to {!pass} but prints "FIXME" to indicate the
        test failed and thus needs to be fixed. *)
  val fail : string -> (string * string) list -> unit
end

(** [check_string_min s m] returns [Ok ()] if [s] is at least of size [m], or an
    error otherwise. *)
val check_string_min : string -> min:int -> (unit, string) Result.t

(** [check_string_max s m] returns [Ok ()] if [s] is at most of size [m], or an
    error otherwise. *)
val check_string_max : string -> max:int -> (unit, string) Result.t

(** [check_pattern s p] returns [Ok ()] if [s] matches with pattern [p], or an
    error otherwise. *)
val check_pattern : string -> pattern:string -> (unit, string) Result.t

(** [check_list_min l m] returns [Ok ()] if [l] is at least of size [m], or an
    error otherwise. *)
val check_list_min : 'a list -> min:int -> (unit, string) Result.t

(** [check_list_max l m] returns [Ok ()] if [l] is at most of size [m], or an
    error otherwise. *)
val check_list_max : 'a list -> max:int -> (unit, string) Result.t

(** [check_int_min i m] returns [Ok ()] if [i] is at least equal to [m], or an
    error otherwise. *)
val check_int_min : int -> min:int -> (unit, string) Result.t

(** [check_int_max i m] returns [Ok ()] if [i] is at most equal to [m], or an
    error otherwise. *)
val check_int_max : int -> max:int -> (unit, string) Result.t

(** [check_int64_min i m f] returns [Ok ()] if [i] is at least equal to [m], or
    an error otherwise. *)
val check_int64_min : int64 -> min:int64 -> (unit, string) Result.t

(** [check_int64_min i m f] returns [Ok ()] if [i] is at most equal to [m], or
    an error otherwise. *)
val check_int64_max : int64 -> max:int64 -> (unit, string) Result.t

(** [check_float_min i m] returns [Ok ()] if [i] is at least equal to [m], or an
    error otherwise. *)
val check_float_min : float -> min:float -> (unit, string) Result.t

(** [check_float_max i m] returns [Ok ()] if [i] is at most equal to [m], or an
    error otherwise. *)
val check_float_max : float -> max:float -> (unit, string) Result.t

(** [string_of_xml ~kind node] checks if [node] represents a string, and returns
    it. Otherwise, it returns an error (mentioning the [kind] of value being
    parsed). *)
val string_of_xml : kind:string -> ('a Xmlm.frag as 'a) Xmlm.frag -> string

(** Convert a JSON value to a boolean (or fail with an exception). *)
val bool_of_json : Json.t -> bool

(** Convert a JSON value to a float (or fail with an exception mentioning the
    [kind] of value being parsed). *)
val float_of_json : kind:string -> Json.t -> float

(** Convert a JSON value to a string (or fail with an exception mentioning the
    [kind] of value being parsed). *)
val string_of_json : kind:string -> Json.t -> string

(** Convert a JSON value to a timestamp (or fail with an exception). *)
val timestamp_of_json : Json.t -> string

(** Convert a JSON value to a list (or fail with an exception mentioning the
    [kind] of value being parsed). *)
val list_of_json : kind:string -> of_json:(Json.t -> 'a) -> [> Json.t ] -> 'a list

(** Convert a JSON object to an association list (or fail with an exception). *)
val object_of_json
  :  key_of_string:(string -> 'key)
  -> of_json:(Json.t -> 'value)
  -> Json.t
  -> ('key * 'value) list
