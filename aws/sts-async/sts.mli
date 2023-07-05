module Statement : sig
  type effect =
    | Accept
    | Deny
  [@@deriving sexp]

  type action = string list [@@deriving sexp]
  type resource = string list [@@deriving sexp]

  val effect_to_string : effect -> string

  type t =
    { sid : string
    ; effect : effect
    ; action : action
    ; resource : resource
    }
  [@@deriving sexp]

  val create
    :  ?effect:effect
    -> ?sid:string
    -> action:action
    -> resource:resource
    -> unit
    -> t

  val to_json : t -> Yojson.Safe.t
end

module Policy : sig
  type t =
    { version : string
    ; statement : Statement.t list
    }
  [@@deriving sexp]

  val to_json : t -> Awsm.Json.t
  val create : ?version:string -> Statement.t list -> t
end

val assume_role
  :  ?policy:Policy.t
  -> ?retry_delay:Time.Span.t
  -> ?retry_cnt:int
  -> ?duration_sec:int
  -> session_name:string
  -> role:string
  -> Awsm.Cfg.t
  -> Values.AssumeRoleResponse.t Deferred.t

val assume_role_with_saml
  :  ?policy:Policy.t
  -> ?retry_delay:Time.Span.t
  -> ?retry_cnt:int
  -> ?duration_sec:int
  -> principal_arn:string
  -> saml_assertion:string
  -> role:string
  -> Awsm.Cfg.t
  -> Values.AssumeRoleWithSAMLResponse.t Deferred.t
