(** Authentication using
    {{:http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html}AWS's
    Signature Version 4} signing process. *)

open! Import

type payload_hash = private string * int

val payload_hash : string -> payload_hash
val empty_payload_hash : payload_hash

module Date_header : sig
  val to_value : Time.t -> string
  val value_of_now : unit -> string
end

module Session_token_header : sig
  val add : string -> Cohttp.Header.t -> Cohttp.Header.t
end

val headers_with_date_and_payload_hash
  :  ?session_token:string
  -> timestamp:Time.t
  -> payload_hash:[ `Unsigned | `Signed of payload_hash ]
  -> Cohttp.Header.t
  -> Cohttp.Header.t

(*
val dup_headers_fixup : Cohttp.Header.t -> Cohttp.Header.t
*)

(** Create a signed URL, i.e. one of the methods of adding signing information
    to a request as defined by
    {{:http://docs.aws.amazon.com/general/latest/gr/sigv4-add-signature-to-request.html}Task
    4}. Result is the original URL with additional query parameters added to it.

    [headers] must contain at least "host".

    [timeout] is the number of seconds after which the signed URL will become
    inactive. Error if value is not between 1 and 604800. Omitting it is
    equivalent to setting 604800, according to AWS's documentation. *)
val sign_url
  :  http_method:Cohttp.Code.meth
  -> region:Region.t
  -> service:Service.t
  -> timestamp:Time.t
  -> headers:Cohttp.Header.t
  -> ?aws_secret_access_key:string
  -> ?aws_access_key_id:string
  -> payload_hash:[ `Unsigned | `Signed of payload_hash ]
  -> ?timeout:int
  -> Uri.t
  -> Uri.t

(** Sign the given request. Named arguments are all the additional information
    needed to sign a request. Returned request is identical to given one, with
    an extra "Authorization" header added. Adheres to
    {{:http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html}AWS
    v4 specification}. *)
val sign_request
  :  ?session_token:string
  -> ?aws_access_key_id:string
  -> ?aws_secret_access_key:string
  -> region:Region.t
  -> service:Service.t
  -> payload_hash:payload_hash
  -> Cohttp.Request.t
  -> Cohttp.Request.t

(** {2 Low Level API} *)

type canonical_request = private string
type credential_scope = private string
type string_to_sign = private string
type signature = private string

(** Create canonical request, i.e.
    {{:http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html}Task
    1}.

    [headers] should include all headers that are part of the request, and must
    contain "host". *)
val canonical_request
  :  http_method:Cohttp.Code.meth
  -> uri:Uri.t
  -> headers:Cohttp.Header.t
  -> payload_hash:[ `Unsigned | `Signed of payload_hash ]
  -> canonical_request

val credential_scope
  :  timestamp:Time.t
  -> region:Region.t
  -> service:Service.t
  -> credential_scope

(** Create {i string to sign}, i.e.
    {{:http://docs.aws.amazon.com/general/latest/gr/sigv4-create-string-to-sign.html}Task
    2}. *)
val string_to_sign
  :  canonical_request:canonical_request
  -> credential_scope:credential_scope
  -> timestamp:Time.t
  -> string_to_sign

(** Calculate signature, i.e.
    {{:http://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html}Task
    3}. *)
val signature
  :  ?aws_secret_access_key:string
  -> string_to_sign:string_to_sign
  -> timestamp:Time.t
  -> region:Region.t
  -> service:Service.t
  -> unit
  -> signature

(** Create Authorization header, which is neeeded by one of the techniques for
    signing requests defined by
    {{:http://docs.aws.amazon.com/general/latest/gr/sigv4-add-signature-to-request.html}Task
    4}. *)
val authorization_header
  :  ?aws_access_key_id:string
  -> signature:signature
  -> credential_scope:credential_scope
  -> headers:Cohttp.Header.t
  -> unit
  -> string * string
