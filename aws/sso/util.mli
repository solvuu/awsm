val get_cached_sso_token_file_path : cfg:Awsm.Cfg.t -> string

val get_sso_role_request_and_cfg_exn
  :  cfg:Awsm.Cfg.t
  -> cached_sso_token_file:string
  -> string
  -> Values.GetRoleCredentialsRequest.t * Awsm.Cfg.t

val get_sso_role_request_and_cfg
  :  cfg:Awsm.Cfg.t
  -> cached_sso_token_file:string
  -> string
  -> (Values.GetRoleCredentialsRequest.t * Awsm.Cfg.t, exn) Result.t

val parse_role_credentials_response_exn
  :  ( Values.GetRoleCredentialsResponse.t
     , [ `AWS of Values.GetRoleCredentialsResponse.error
       | `Transport of Awsm.Http.Io.Error.call
       ] )
     Result.t
  -> Values.RoleCredentials.t option

val update_cfg_with_role_credentials_exn
  :  cfg:Awsm.Cfg.t
  -> Values.RoleCredentials.t option
  -> Awsm.Cfg.t

val update_cfg_with_role_credentials
  :  cfg:Awsm.Cfg.t
  -> Values.RoleCredentials.t option
  -> (Awsm.Cfg.t, exn) Result.t
