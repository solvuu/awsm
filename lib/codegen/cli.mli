open! Core
open! Import

val make
  :  submodules:string list
  -> Botodata.service
  -> Parsetree.structure * (string * Parsetree.structure) list
