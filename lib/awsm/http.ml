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

module Meth = struct
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
  [@@deriving sexp_of]

  type t =
    [ standard
    | `Other of string
    ]
  [@@deriving sexp_of]

  let pp ppf x = Sexp.pp_hum ppf (sexp_of_t x)
end

module Headers = struct
  type t = (string, string) List.Assoc.t [@@deriving sexp_of]

  let empty = []
  let of_list x = x
  let to_list x = x
  let pp ppf l = Sexp.pp_hum ppf (sexp_of_t l)
end

module Monad = struct
  type (+'x, 'f) app

  module type S = sig
    type 'a s
    type t

    external inj : 'a s -> ('a, t) app = "%identity"
    external prj : ('a, t) app -> 'a s = "%identity"
  end

  module Make (T : sig
    type 'a t
  end) : S with type 'a s = 'a T.t = struct
    type 'a s = 'a T.t
    type t

    external inj : 'a -> 'b = "%identity"
    external prj : 'a -> 'b = "%identity"
  end

  type 'm t =
    { bind : 'a 'b. ('a, 'm) app -> ('a -> ('b, 'm) app) -> ('b, 'm) app
    ; return : 'a. 'a -> ('a, 'm) app
    }
end

module Range = struct
  type byte_range_spec =
    [ `Range of int64 * int64
    | `From_start of int64
    | `From_end of int64
    ]
  [@@deriving sexp]

  let of_range start stop =
    if Int64.compare stop start < 0
    then
      Error
        (sprintf "Not a valid byte range specification: start=%Ld, stop=%Ld" start stop)
    else Ok [ `Range (start, stop) ]
  ;;

  let%expect_test "of_range" =
    let test x y =
      of_range x y
      |> Result.sexp_of_t (List.sexp_of_t sexp_of_byte_range_spec) sexp_of_string
      |> Sexp.to_string
      |> print_endline
    in
    (* Test 1: first argument < second argument *)
    test Int64.min_value Int64.max_value;
    [%expect {| (Ok((Range(-9223372036854775808 9223372036854775807)))) |}];
    (* Test 2: first argument = second argument *)
    test Int64.min_value Int64.min_value;
    [%expect {| (Ok((Range(-9223372036854775808 -9223372036854775808)))) |}];
    (* Test 3: first argument > second argument, invalid range *)
    test Int64.max_value Int64.min_value;
    [%expect
      {|
      (Error"Not a valid byte range specification: start=9223372036854775807, stop=-9223372036854775808") |}]
  ;;

  let from_end start = [ `From_end start ]
  let from_start start = [ `From_start start ]

  let byte_range_spec_to_string (spec : byte_range_spec) =
    match spec with
    | `Range (start, stop) -> sprintf "%Ld-%Ld" start stop
    | `From_start start -> sprintf "%Ld-" start
    | `From_end stop -> sprintf "-%Ld" stop
  ;;

  let%expect_test "byte_range_spec_to_string" =
    let test x = printf !"%s" (byte_range_spec_to_string x) in
    (* Test 1: `Range (correct by design, no need to check the bounds) *)
    test (`Range (Int64.zero, Int64.one));
    [%expect {| 0-1 |}];
    (* Test 2: `From_start *)
    test (`From_start Int64.one);
    [%expect {| 1- |}];
    (* Test 3: `From_end *)
    test (`From_end Int64.one);
    [%expect {| -1 |}]
  ;;

  type t = byte_range_spec list

  let to_header (t : t) =
    let byte_range_spec_strs = List.map ~f:byte_range_spec_to_string t in
    let concatenated = String.concat ~sep:"," byte_range_spec_strs in
    "Range", sprintf "bytes=%s" concatenated
  ;;

  let%expect_test "to_header" =
    let test x = printf !"%{sexp: (string * string)}" (to_header x) in
    (* Test 1: empty list *)
    test [];
    [%expect {| (Range bytes=) |}];
    (* Test 2: list of a single `Range *)
    test [ `Range (Int64.zero, Int64.one) ];
    [%expect {| (Range bytes=0-1) |}];
    (* Test 3: list of a single `From_start *)
    test [ `From_start Int64.one ];
    [%expect {| (Range bytes=1-) |}];
    (* Test 4: list of a `From_start and a `From_end *)
    test [ `From_start Int64.zero; `From_end Int64.one ];
    [%expect {| (Range bytes=0-,-1) |}]
  ;;

  let to_header_value (t : t) =
    let k, v = to_header t in
    sprintf "%s: %s" k v
  ;;

  let%expect_test "to_header_value" =
    let test x = printf !"%s" (to_header_value x) in
    (* Test 1: empty list *)
    test [];
    [%expect {| Range: bytes= |}];
    (* Test 2: list of a single `Range *)
    test [ `Range (Int64.zero, Int64.one) ];
    [%expect {| Range: bytes=0-1 |}];
    (* Test 3: list of a single `From_start *)
    test [ `From_start Int64.one ];
    [%expect {| Range: bytes=1- |}];
    (* Test 4: list of a `From_start and a `From_end *)
    test [ `From_start Int64.zero; `From_end Int64.one ];
    [%expect {| Range: bytes=0-,-1 |}]
  ;;
end

module Status = struct
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

module Request = struct
  type t =
    { meth : Meth.t
    ; version : int * int
    ; headers : Headers.t
    ; body : string
    }
  [@@deriving fields, sexp_of]

  let make ?(version = 1, 1) ?(headers = Headers.empty) ?(body = "") meth =
    { meth; version; headers; body }
  ;;

  let pp ppf t = Sexp.pp_hum ppf (sexp_of_t t)
end

module Response = struct
  type 's t =
    { version : int * int
    ; status : Status.t
    ; reason : string
    ; headers : Headers.t
    ; body : (string, 's) stream
    }

  and ('a, 's) stream = unit -> ('a option, 's) Monad.app

  let make ?(version = 1, 1) ?(reason = "") ?(headers = Headers.empty) ~body status =
    { version; reason; headers; status; body }
  ;;

  let status { status; _ } = status
  let headers { headers; _ } = headers
  let body { body; _ } = body
  let version { version; _ } = version
  let reason { reason; _ } = reason

  let body_to_string ?(buffer = 0x1000) { Monad.return; bind } { body; _ } =
    let ( >>= ) = bind in
    let buf = Buffer.create buffer in
    let rec go () =
      body ()
      >>= function
      | Some x ->
        Buffer.add_string buf x;
        go ()
      | None -> return (Buffer.contents buf)
    in
    go ()
  ;;
end

module Call = struct
  type ('s, 'error) t =
    Meth.t -> Request.t -> Uri.t -> (('s Response.t, 'error) result, 's) Monad.app
end

module Io = struct
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
