(** Amazon Resource Names. See AWS's documentation
    {{:https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html}Amazon
    Resource Names (ARNs) and AWS Service Namespaces} for details, but roughly
    ARNs are strings of the form:

    {v arn:partition:service:region:account-id:resourcetype/resource/qualifier
    v}

    We provide a type {!t} deconstructing this string into its components.
    Functions {!to_string} and {!of_string} print and parse strings according to
    the AWS specification.

    Additionally, we provide functions for constructing ARNs specific to a
    service, e.g. {!s3}. These are more convenient because certain services
    don't require or disallow certain components of a general ARN, so these
    functions ask for only what is needed. Also, ARNs may have additional
    requirements for a given service, which these functions may enforce (though
    we don't yet). *)

open! Import

module Error : sig
  type make =
    [ `Invalid_qualifier of string
    | `Invalid_account_id of string
    ]
  [@@deriving sexp]
end

type resource_type =
  [ `Slash_delimited of string
  | `Colon_delimited of string
  | `None
  ]
[@@deriving sexp]

type qualifier =
  [ `Slash_delimited of string
  | `Colon_delimited of string
  | `None
  ]
[@@deriving sexp]

type t =
  { partition : string
  ; service : Service.t
  ; region : Region.t option
  ; account_id : string option
  ; resource : string
  ; resource_type : resource_type
  ; qualifier : qualifier
  }
[@@deriving sexp, compare]

(** Make an ARN. Defaults: - partition: {!Default.partition} - region: None -
    account_id: None - resource_type: `None - qualifier: `None *)
val make
  :  ?partition:string
  -> ?region:Region.t
  -> ?account_id:string
  -> ?resource_type:resource_type
  -> ?qualifier:qualifier
  -> service:Service.t
  -> resource:string
  -> unit
  -> [ `Ok of t | Error.make ]

(** [to_string t] prints a valid ARN string. *)
val to_string : t -> string

(** [of_string s] parses an ARN string according to the AWS specification.
    @raise Invalid_arn_format *)
val of_string : string -> t

(** Produce an S3 ARN. Default [partition] is {!Default.partition}. *)
val s3
  :  ?partition:string
  -> bucket:string
  -> key:string
  -> unit
  -> [ `Ok of t | `Invalid_bucket of string | `Invalid_key of string ]

module Default : sig
  (** "aws" *)
  val partition : string
end

module Exn : sig
  (** @raise Failure *)
  val make
    :  ?partition:string
    -> ?region:Region.t
    -> ?account_id:string
    -> ?resource_type:resource_type
    -> ?qualifier:qualifier
    -> service:Service.t
    -> resource:string
    -> unit
    -> t

  (** @raise Failure *)
  val s3 : ?partition:string -> bucket:string -> key:string -> unit -> t
end
