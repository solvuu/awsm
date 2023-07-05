open! Import

type t = ('a Xmlm.frag as 'a) Xmlm.frag

val child : t -> string -> t option
val child_exn : ?context:string -> t -> string -> t
val children : t -> string -> t list
val all_children : t -> t list
val string_data_exn : ?context:string -> t -> string
val to_string : t -> string
val of_value : string -> Botodata.value -> t list
val of_values : string -> (string * Botodata.value) list -> t
val parse_response : string -> t

val from_string
  :  string
  -> Xmlm.dtd * ([> `Data of string | `El of Xmlm.tag * 'a list ] as 'a) list
