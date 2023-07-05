open! Core
open! Import

val xml_ty : Botodata.shape -> Parsetree.core_type
val of_xml_return_ty : Botodata.shape -> Parsetree.core_type
val of_json_arg_ty : Botodata.shape -> Parsetree.core_type
val to_json_return_ty : Botodata.shape -> Parsetree.core_type
