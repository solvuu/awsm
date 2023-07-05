(** AWS services, such as S3, EC2, IAM, etc. *)

open! Import
include module type of Awsm_codegen.Service
