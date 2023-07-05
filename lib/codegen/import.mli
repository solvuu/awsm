open! Core

module Process : sig
  module Output : sig
    type t =
      { exit_status : Core_unix.Exit_or_signal.t
      ; stdout : string
      ; stderr : string
      }
  end

  val run : prog:string -> args:string list -> (Output.t, exn) result
end

module Util : sig
  (** [mkdir_exn dir] creates the directory [dir] if it doesn't already exist. Raises if
      unable to determine the status of [dir] or if [dir] already exists but is a regular
      file. *)
  val mkdir_exn : string -> unit

  (** [camel_to_snake_case x] assumes string [x] is in camel case, and converts it to
      snake case. Example: [camel_to_snake_case "AbortMultipartUpload"] returns
      ["abort_multipart_upload"]. *)
  val camel_to_snake_case : ?sep:char -> string -> string

  val tokenize
    :  (Sedlexing.lexbuf -> ('a option, 'err) result)
    -> string
    -> ('a list, 'err) result

  (** Given a printer [f] that prints to a formatter, return a [to_string] function that
    works on the same type of values. *)
  val to_string_of_printer : (Format.formatter -> 'a -> unit) -> 'a -> string

  val structure_to_string : Parsetree.structure -> string
  val signature_to_string : Parsetree.signature -> string
  val expression_to_string : Parsetree.expression -> string
  val core_type_to_string : Parsetree.core_type -> string
end
