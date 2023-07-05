open! Core
open! Import

val make
  :  awsm_service_id:string
  -> submodules:string list
  -> Botodata.service
  -> Parsetree.structure * (string * Parsetree.structure) list
