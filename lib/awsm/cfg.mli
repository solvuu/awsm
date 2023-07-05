(** A configuration defines parameters needed for making calls to AWS. Some parameters,
    such as access keys and region, are needed by virtually all calls, while others
    apply only to specific services. We strive to match the
    {{:https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html}specifications}
    of the [aws] CLI. It defines multiple sources from which values of these parameters
    can be obtained---[config] and [credentials] files, environment variables, and
    command line flags---and selects values from these sources in a certain precedence
    order. The main [make] constructor considers all such sources and applies the same
    precedence rules. Submodules [Config_file] and [Credentials_file] provide parsers for
    the [config] and [credentials] files, respectively.

    This module should not be confused with the
    {{:http://aws.amazon.com/config/}AWS Config} service. *)

open! Import

module S3_custom_command_settings : sig
  (* We don't use these in awsm ourselves, but the .aws/config format allows
     for configuring the aws s3 higher-level command this way. So, we parse it.*)
  type t =
    { max_concurrent_requests : string option
    ; max_queue_size : string option
    ; multipart_threshold : string option
    ; multipart_chunksize : string option
    ; max_bandwidth : string option
    ; use_accelerate_endpoint : string option
    ; use_dualstack_endpoint : string option
    ; addressing_style : string option
    }
  [@@deriving fields, sexp]
end

type t =
  { aws_access_key_id : string option
  ; aws_secret_access_key : string option
  ; aws_session_token : string option
  ; region : Region.t option
  ; output : string option
  ; ca_bundle : string option
  ; cli_auto_prompt : string option
  ; cli_binary_format : string option
  ; cli_history : string option
  ; cli_pager : string option
  ; cli_timestamp_format : string option
  ; credential_process : string option
  ; credential_source : string option
  ; duration_seconds : string option
  ; external_id : string option
  ; max_attempts : string option
  ; mfa_serial : string option
  ; parameter_validation : string option
  ; retry_mode : string option
  ; role_arn : string option
  ; role_session_name : string option
  ; source_profile : string option
  ; sso_account_id : string option
  ; sso_region : string option
  ; sso_role_name : string option
  ; sso_start_url : string option
  ; web_identity_token_file : string option
  ; tcp_keepalive : string option
  ; s3_custom_command_settings : S3_custom_command_settings.t option
  }
[@@deriving sexp]

val empty : t

module Config_file : sig
  type t

  (** [of_string x] parses the content of a config file in the format specified by the AWS CLI. *)
  val of_string : string -> (t, string) Result.t

  (** [path ()] returns the path to the config file following the specifications of the
      [aws] CLI. *)
  val path : unit -> string option
end

module Shared_credentials_file : sig
  type t

  (** [of_string x] parses the content of a shared credentials file in the format specified by the AWS CLI. *)
  val of_string : string -> (t, string) Result.t

  (** [path ()] returns the path to the credentials file following the specifications of the
      [aws] CLI. *)
  val path : unit -> string option
end

val make
  :  ?config_file:string * Config_file.t
  -> ?shared_credentials_file:string * Shared_credentials_file.t
  -> ?profile:string
  -> ?aws_access_key_id:string
  -> ?aws_secret_access_key:string
  -> ?region:Region.t
  -> ?output:string
  -> unit
  -> (t, string) Result.t
