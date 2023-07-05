(** Types representing content of botocore/data/endpoints.json. Type [t]
    represents the full file.

    When posting a request, an AWS client has to determine the exact URI
    the request has to be sent to. This depends on the targeted service and
    the region. The rules are not systematically provided by Amazon, but
    botocore provides an endpoints.json file with a set of heuristic rules
    that can be used to determine a proper endpoint for a request.
*)
open! Core

open! Import

(* Warning 30 complains if we have 'defaults' in multiple fields. *)
[@@@warning "-30"]

type credentialScope =
  { region : Region.t option
  ; service : string option
  }
[@@deriving sexp]

type uri_token =
  [ `String_token of string
  | `Service_token
  | `Region_token
  | `DnsSuffix_token
  ]
[@@deriving sexp]

type uri_pattern = uri_token list [@@deriving sexp]

type variant =
  { dnsSuffix : string option
  ; hostname : uri_pattern option
  ; tags : string list
  }
[@@deriving sexp]

type properties =
  { credentialScope : credentialScope option
  ; hostname : uri_pattern option
  ; protocols : string list option
  ; sslCommonName : string option
  ; signatureVersions : [ `v2 | `v3 | `v4 | `s3 | `s3v4 ] list option
  ; variants : variant list option
  ; deprecated : bool option
  }
[@@deriving sexp]

type service =
  { defaults : properties option
  ; endpoints : (string * properties) list
  ; isRegionalized : bool option
  ; partitionEndpoint : string option
  }
[@@deriving sexp]

type region = { description : string } [@@deriving sexp]

type partition =
  { defaults : properties option
  ; dnsSuffix : string
  ; partition : string
  ; partitionName : string
  ; regionRegex : string
  ; regions : (string * region) list
  ; services : (string * service) list
  }
[@@deriving sexp]

type t =
  { partitions : partition list
  ; version : int
  }
[@@deriving sexp]

val of_json : string -> t

(** [make_lookup_uri e] generates a module implementation for endpoint heuristics,
    which are rules described in botodata to determine an endpoint (service URL, basically)
    for a service given a region and a few more properties. *)
val make_lookup_uri : t -> structure_item

(** [make_lookup_credential_scope e] is similar to the above, but determines
    the region credentials should be bound to, though most of the time it's the same as
    the region of the service. One notable exception is route53. *)
val make_lookup_credential_scope : t -> structure_item
