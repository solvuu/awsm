(** Parsers for Amazon service specification from boto library

    This module can be used to parse the JSON-encoded specification of Amazon
    services provided in the boto library.

    The specification files are provided in
    {{:https://github.com/boto/botocore/tree/develop/botocore/data} boto
    library}, and the function {! traversal} can be used to read all the
    specifications from local copy of boto source code. *)
open! Core

open! Import

(** [to_ocaml x] converts a service name in Botocore format to a value in
    [Awsm.Service]. *)
val to_ocaml : string -> expression

module TimestampFormat : sig
  type t = Botodata.timestampFormat

  val parser : t Json_parser.t
end

module Location : sig
  type t = Botodata.location

  val parser : t Json_parser.t
end

module Uri : sig
  type t = Uri.t

  val parser : t Json_parser.t
end

module Metadata : sig
  module ChecksumFormat : sig
    type t = Botodata.checksumFormat

    val parser : t Json_parser.t
  end

  module Protocol : sig
    type t = Botodata.protocol

    val parser : t Json_parser.t
  end

  type t = Botodata.metadata

  val parser : t Json_parser.t
end

(** Parse a string into a [requestUri] (list of tokens) or raise [Failure]. *)
val parse_requestUri : string -> Botodata.requestUri

module XmlNamespace : sig
  type t = Botodata.xmlNamespace

  val parser : t Json_parser.t
end

module Error : sig
  type t = Botodata.error

  val parser : t Json_parser.t
end

module Operation : sig
  module Input : sig
    type t = Botodata.operation_input

    val parser : t Json_parser.t
  end

  module Output : sig
    type t = Botodata.operation_output

    val parser : t Json_parser.t
  end

  module Error : sig
    type t = Botodata.operation_error

    val parser : t list Json_parser.t
  end

  module Endpoint : sig
    type t = Botodata.operation_endpoint

    val parser : t Json_parser.t
  end

  module Endpointdiscovergy : sig
    type t = Botodata.operation_endpointdiscovery

    val parser : t Json_parser.t
  end

  module Http : sig
    type t = Botodata.http

    val parser : t Json_parser.t
  end

  module HttpChecksum : sig
    type t = Botodata.httpChecksum

    val parser : t Json_parser.t
  end

  type t = Botodata.operation

  val parser : t Json_parser.t
end

module Operations : sig
  type t = Botodata.operation list

  val parser : t Json_parser.t
end

module Shape : sig
  module Bool : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Blob : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Long : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Double : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Float : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Integer : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Timestamp : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Member : sig
    type t = Botodata.shape_member

    val parser : t Json_parser.t
  end

  module List : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Map : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Retryable : sig
    type t = Botodata.retryable

    val parser : t Json_parser.t
  end

  module Structure : sig
    val parser : Botodata.shape Json_parser.record
  end

  module Enum : sig
    val parser : Botodata.shape Json_parser.record
  end

  module String : sig
    val parser : Botodata.shape Json_parser.record
  end

  type t = Botodata.shape

  val parser : t Json_parser.t
end

module Shapes : sig
  type t = (string * Botodata.shape) list

  val parser : t Json_parser.t
end

type t = Botodata.service

val parse : Json.t -> t
val of_json : string -> t
