open! Core
open! Import

val structure_item_of_shape
  :  ?result_wrapper:string
  -> Botodata.shape
  -> Parsetree.structure_item

val type_of_shape : Botodata.shape -> Parsetree.core_type
val private_flag_of_shape : Botodata.shape -> Asttypes.private_flag
