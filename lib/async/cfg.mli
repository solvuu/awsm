open! Import

(** Get a configuration, following a provider chain as follows:

      - First search for a config file by taking the first one found in the
      following order: [config_file] argument to this function if any,
      environment variable AWS_CONFIG_FILE if set, and finally "~/.aws/config".
      It is okay for none to exist.

      - Within the config file, settings are chosen from the specified
      [profile], which is taken as the first one of the following: [profile]
      argument to this function if given, environment variable
      AWS_DEFAULT_PROFILE if set, and finally "default".

      - Then each config parameter is set to the first value found in the
      following order: corresponding argument to this function, corresponding
      environment variable (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY,
      AWS_SESSION_TOKEN, and AWS_DEFAULT_REGION), or value from the profile
      found above. *)
val get
  :  ?profile:string
  -> ?aws_access_key_id:string
  -> ?aws_secret_access_key:string
  -> ?region:Awsm.Region.t
  -> ?output:string
  -> unit
  -> (Awsm.Cfg.t, string) Result.t Deferred.t

val get_exn
  :  ?profile:string
  -> ?aws_access_key_id:string
  -> ?aws_secret_access_key:string
  -> ?region:Awsm.Region.t
  -> ?output:string
  -> unit
  -> Awsm.Cfg.t Deferred.t
