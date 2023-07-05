(** Uniform Cohttp interface. Cohttp's interface isn't entirely uniform, making
    it difficult to provide a single functorized implementation over Async and
    Lwt. We define a signature here that covers the features of Cohttp we need.
    Async and Lwt implementations matching this signature are provided. *)

open! Import

module type S = sig
  module Deferred : sig
    type 'a t
  end

  module Pipe : sig
    module Reader : sig
      type 'a t
    end

    module Flushed : sig
      type t
    end

    val fold
      :  ?flushed:Flushed.t
      -> 'a Reader.t
      -> init:'accum
      -> f:('accum -> 'a -> 'accum Deferred.t)
      -> 'accum Deferred.t

    val of_list : 'a list -> 'a Reader.t
  end

  module Response : sig
    type t

    val status : t -> Cohttp.Code.status_code
    val headers : t -> (string * string) list
  end

  module Body : sig
    type t

    val of_pipe : string Pipe.Reader.t -> t
    val of_string : string -> t
    val to_string : t -> string Deferred.t
    val to_pipe : t -> string Pipe.Reader.t
  end

  module Client : sig
    val get : ?headers:Cohttp.Header.t -> Uri.t -> (Response.t * Body.t) Deferred.t

    val post
      :  ?headers:Cohttp.Header.t
      -> ?body:Body.t
      -> ?chunked:bool
      -> Uri.t
      -> (Response.t * Body.t) Deferred.t

    val put
      :  ?headers:Cohttp.Header.t
      -> ?body:Body.t
      -> ?chunked:bool
      -> Uri.t
      -> (Response.t * Body.t) Deferred.t

    val delete
      :  ?headers:Cohttp.Header.t
      -> ?body:Body.t
      -> ?chunked:bool
      -> Uri.t
      -> (Response.t * Body.t) Deferred.t
  end
end

module Meth : sig
  type standard =
    [ `GET
    | `HEAD
    | `POST
    | `PUT
    | `DELETE
    | `CONNECT
    | `OPTIONS
    | `TRACE
    | `PATCH
    ]

  type t =
    [ standard
    | `Other of string
    ]
  [@@deriving sexp_of]

  val pp : Format.formatter -> t -> unit
end

module Headers : sig
  type t [@@deriving sexp_of]

  val pp : Format.formatter -> t -> unit
  val empty : t
  val of_list : (string * string) list -> t
  val to_list : t -> (string * string) list
end

module Monad : sig
  (** Monad type class. The technique used here relies on lightweight higher-kinded
    polymorphism as described by Yallop and White in FLOPS (2014). Familiarity
    with the concepts in that paper is a prerequisite to understanding this
    module. However, most users do not need to understand this module since
    common useful instances of the abstract types here are provided elsewhere. *)

  (* Type level application. Conceptually represents the type ['x 'f]. *)
  type (+'x, 'f) app

  module type S = sig
    (** The underlying actual monad type. *)
    type 'a s

    (** Monad type lifted into the [app] type. *)
    type t

    external inj : 'a s -> ('a, t) app = "%identity"
    external prj : ('a, t) app -> 'a s = "%identity"
  end

  module Make (T : sig
    type 'a t
  end) : S with type 'a s = 'a T.t

  (** Monad type class. The monad type is the type parameter ['m], and the
    monadic operations are provided as a dictionary of functions. *)
  type 'm t =
    { bind : 'a 'b. ('a, 'm) app -> ('a -> ('b, 'm) app) -> ('b, 'm) app
    ; return : 'a. 'a -> ('a, 'm) app
    }
end

module Range : sig
  (** High level representation of a http byte range specifications. For a full
    treatment, see
    https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 *)

  type byte_range_spec
  type t = byte_range_spec list

  (** [of_range start stop] returns [Error] if [stop] < [start]. *)
  val of_range : int64 -> int64 -> (t, string) result

  val from_end : int64 -> t
  val from_start : int64 -> t
  val to_header_value : t -> string
  val to_header : t -> string * string
end

module Status : sig
  type informational =
    [ `Continue
    | `Switching_protocols
    ]
  [@@deriving sexp_of]

  type successful =
    [ `OK
    | `Created
    | `Accepted
    | `Non_authoritative_information
    | `No_content
    | `Reset_content
    | `Partial_content
    ]
  [@@deriving sexp_of]

  type redirection =
    [ `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Use_proxy
    | `Temporary_redirect
    ]
  [@@deriving sexp_of]

  type client_error =
    [ `Bad_request
    | `Unauthorized
    | `Payment_required
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Not_acceptable
    | `Proxy_authentication_required
    | `Request_timeout
    | `Conflict
    | `Gone
    | `Length_required
    | `Precondition_failed
    | `Unsupported_media_type
    | `Expectation_failed
    | `I_m_a_teapot
    | `Enhance_your_calm
    | `Upgrade_required
    ]
  [@@deriving sexp_of]

  type server_error =
    [ `Internal_server_error
    | `Not_implemented
    | `Bad_gateway
    | `Service_unavailable
    | `Gateway_timeout
    | `Http_version_not_supported
    ]
  [@@deriving sexp_of]

  type standard =
    [ informational
    | successful
    | redirection
    | client_error
    | server_error
    ]
  [@@deriving sexp_of]

  type t =
    [ standard
    | `Code of int
    ]
  [@@deriving sexp_of]
end

module Request : sig
  type t

  val version : t -> int * int
  val headers : t -> Headers.t
  val meth : t -> Meth.t
  val body : t -> string
  val make : ?version:int * int -> ?headers:Headers.t -> ?body:string -> Meth.t -> t
  val pp : Format.formatter -> t -> unit
end

module Response : sig
  type 's t
  and ('a, 's) stream = unit -> ('a option, 's) Monad.app

  val version : 's t -> int * int
  val reason : 's t -> string
  val status : 's t -> Status.t
  val headers : 's t -> Headers.t
  val body : 's t -> (string, 's) stream
  val body_to_string : ?buffer:int -> 's Monad.t -> 's t -> (string, 's) Monad.app

  val make
    :  ?version:int * int
    -> ?reason:string
    -> ?headers:Headers.t
    -> body:(string, 's) stream
    -> Status.t
    -> 's t
end

module Call : sig
  type ('s, 'error) t =
    Meth.t -> Request.t -> Uri.t -> (('s Response.t, 'error) result, 's) Monad.app
end

module Io : module type of struct
  module type S = sig
    include Monad.S

    type 'a stream

    val monad : t Monad.t
    val make_stream : 'a stream -> ('a, t) Response.stream

    val make_http
      :  (Meth.t -> Request.t -> Uri.t -> (t Response.t, 'error) result s)
      -> (t, 'error) Call.t
  end

  module Error = struct
    type bad_response =
      { code : int
      ; body : string
      ; x_amzn_error_type : string option
      }
    [@@deriving sexp]

    type call =
      [ `Bad_response of bad_response
      | `Too_many_redirects
      ]
    [@@deriving sexp]
  end
end
