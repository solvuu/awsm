open! Awsm.Import

module Attribute : sig
  type raw_attribute =
    { name : string
    ; value : string option
    }
  [@@deriving sexp]

  type t =
    [ `Unknown of raw_attribute
    | `Custom of raw_attribute
    | `Gender of string
    | `Family_name of string
    | `Locale of string
    | `Middle_name of string option
    | `Nickname of string
    | `Profile of string option
    | `Website of string option
    | `Picture of string
    | `Email of string
    | `Name of string
    | `Updated_at of string
    | `Preferred_user_name of string option
    | `Given_name of string
    ]
  [@@deriving sexp]
end

type attribute = Attribute.t [@@deriving sexp]

type t =
  { username : string
  ; attributes : attribute list
  ; access_token : string
  }
[@@deriving sexp]

type msg = string

(** [required_attribute x ~name ~f] looks for the attribute [a] named [name] in
    the attributes of [x]. If it is found, returns [f] applied to this
    attribute, otherwise returns an error message. *)
val required_attribute
  :  t
  -> name:string
  -> f:(attribute -> 'a option)
  -> ('a, msg) result

(** @raise Failure *)
val required_attribute_exn : t -> name:string -> f:(attribute -> 'a option) -> 'a

(** [optional_attribute x ~f] looks for the attribute satisfying [f] in the
    attributes of [x]. If it is found, returns [f] applied to this attribute,
    otherwise returns [None]. *)
val optional_attribute : t -> f:(attribute -> 'a option) -> 'a option

(** [email x] looks for the email attribute of [x]. If it is found, returns the
    email string, otherwise returns an error message. *)
val email : t -> (string, msg) result

(** @raise Failure *)
val email_exn : t -> string

(** [preferred_name x] returns the value of the preferred_name attribute of [x]
    if it exists, otherwise returns [None]. *)
val preferred_name : t -> string option option

(** [family_name x] looks for the family_name attribute of [x]. If it is found,
    returns the family_name string, otherwise returns an error message. *)
val family_name : t -> (string, msg) result

(** @raise Failure *)
val family_name_exn : t -> string

(** [name x] returns the value of the name attribute of [x] if it exists,
    otherwise returns [None]. *)
val name : t -> string option

module Exn : sig
  (** @raise Failure *)
  val required_attribute : t -> name:string -> f:(attribute -> 'a option) -> 'a

  (** @raise Failure *)
  val email : t -> string

  (** @raise Failure *)
  val family_name : t -> string
end
