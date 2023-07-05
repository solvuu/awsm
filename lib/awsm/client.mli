module Query : sig
  type t =
    | String of string
    | List of t list
    | Structure of (string * t) list
    | Map of (string * string) list

  type value =
    [ `Blob of string
    | `Boolean of bool
    | `Double of Float.t
    | `Enum of string
    | `Float of Float.t
    | `Integer of int
    | `List of value list
    | `Long of Int64.t
    | `Map of (value * value) list
    | `String of string
    | `Structure of (string * value) list
    | `Timestamp of string
    ]

  val of_value : value -> t
  val render : t -> (String.t * string list) list
end

val content_md5 : string -> string
