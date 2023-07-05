open! Core
open! Import

val shape_is_header_structure : Botodata.service -> Botodata.shape -> bool

val shape_is_header_structure'
  :  shapes:(string * Botodata.shape) list
  -> Botodata.shape
  -> bool

val capitalized_id : string -> string
val uncapitalized_id : string -> string
val structure_shape_required_field : Botodata.structure_shape -> string -> bool
val core_type_of_shape : string -> Parsetree.core_type
val response_metadata_shape_name : string

module Graph : sig
  type t

  val of_service : Botodata.service -> t

  module Dfs : sig
    val has_cycle : t -> bool
  end

  module Topological : sig
    val fold : (string -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Components : sig
    val fold
      :  on_scc:('a -> 'b -> 'a)
      -> on_vertex:(string -> with_sig:bool -> 'b -> 'b)
      -> init_scc:'a
      -> init_vertex:'b
      -> t
      -> 'a
  end
end
