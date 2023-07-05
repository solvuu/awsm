module type S = sig
  type ty
  type 'a io
  type t = ty [@@deriving sexp]

  val make : ty -> t
  val to_value : t -> Botodata.value io
  val to_query : t -> Client.Query.t io
  val to_header : t -> string io
  val of_xml : Xml.t -> t
  val of_json : Json.t -> t
  val to_json : t -> Json.t io
  val of_string : string -> t
end
