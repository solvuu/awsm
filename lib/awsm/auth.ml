open! Import

type payload_hash = string * int
type canonical_request = string
type credential_scope = string
type string_to_sign = string
type signature = string

let service_to_string_no_api_prefix service =
  let s = Service.to_string service in
  let prefix = "api." in
  match String.is_prefix s ~prefix with
  | true -> String.chop_prefix_exn s ~prefix
  | false -> s
;;

module Payload_header = struct
  let x_amz_content_sha256_key = "x-amz-content-sha256"

  let to_value = function
    | `Streaming -> "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"
    | `Signed (hash, (_ : int)) -> hash
    | `Unsigned -> "UNSIGNED-PAYLOAD"
  ;;

  let add t header = Cohttp.Header.add header x_amz_content_sha256_key (to_value t)
end

module Date_header = struct
  let datestamp (timestamp : Time.t) : string =
    let date = Time.to_date timestamp ~zone:Time.Zone.utc in
    Date.to_string_iso8601_basic date
  ;;

  let amzdate (timestamp : Time.t) : string =
    let date, time_ofday = Time.to_date_ofday timestamp ~zone:Time.Zone.utc in
    let datestamp = Date.to_string_iso8601_basic date in
    let { Time.Span.Parts.hr; min; sec; _ } = Time.Ofday.to_parts time_ofday in
    sprintf "%sT%02d%02d%02dZ" datestamp hr min sec
  ;;

  let x_amz_date_key = "x-amz-date"
  let to_value timestamp = amzdate timestamp
  let add t header = Cohttp.Header.add header x_amz_date_key (to_value t)
  let value_of_now () = amzdate (Time.now ())
end

module Session_token_header = struct
  let x_amz_security_token = "x-amz-security-token"
  let to_value t = t
  let add t header = Cohttp.Header.add header x_amz_security_token (to_value t)
end

(******************************************************************************)
(* Cryptography                                                               *)
(******************************************************************************)
let algorithm = "AWS4-HMAC-SHA256"
let sha256 s = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s
let hex_encode s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
let hmac_sha256 ~key ~data = Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 key) data
let payload_hash s = hex_encode (sha256 s), String.length s
let empty_payload_hash = payload_hash ""

(******************************************************************************)
(* Low Level API                                                              *)
(******************************************************************************)

let rec filter_except_last ~f = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs ->
    let t = filter_except_last ~f xs in
    if f x then x :: t else t
;;

let filter_middle ~f l =
  match l with
  | [] -> []
  | x :: xs -> x :: filter_except_last ~f xs
;;

let normalize_path_components s =
  String.split s ~on:'/'
  |> filter_middle ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:"/"
;;

let canonical_uri_path uri =
  let slash_by_default = function
    | "" -> "/"
    | x -> x
  in
  Uri.path uri |> normalize_path_components |> slash_by_default
;;

let%expect_test "canonical_uri_path" =
  let test path = print_string (canonical_uri_path (Uri.make ~path ())) in
  test "";
  [%expect {| / |}];
  test "/";
  [%expect {| / |}];
  test "/x/y";
  [%expect {| /x/y |}];
  test "/x/y/";
  [%expect {| /x/y/ |}];
  test "/x//y";
  [%expect {| /x/y |}]
;;

let canonical_uri_encode_char = function
  | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~') as c ->
    Char.to_string c
  | c -> sprintf "%%%X" (Char.to_int c)
;;

let canonical_uri_encode x = String.concat_map x ~f:canonical_uri_encode_char

let canonical_uri_query : Uri.t -> string =
 fun uri ->
  Uri.query uri
  |> List.sort ~compare:[%compare: string * string list]
  |> List.map ~f:(fun (x, ys) ->
       sprintf
         "%s=%s"
         (canonical_uri_encode x)
         (List.map ys ~f:canonical_uri_encode |> String.concat ~sep:","))
  |> String.concat ~sep:"&"
;;

let%expect_test "canonical_uri_query" =
  let test s = print_string (canonical_uri_query (Uri.of_string s)) in
  test "";
  [%expect];
  test "/?k=v";
  [%expect {| k=v |}];
  test "/?y=yv&x=zx";
  [%expect {| x=zx&y=yv |}];
  test "/?a=y&a=x";
  [%expect {| a=x&a=y |}];
  test "/?\225\136\180=v";
  [%expect {| %E1%88%B4=v |}]
;;

let canonical_headers headers : (string * string list) list =
  Cohttp.Header.to_list headers
  |> String.Caseless.Map.of_alist_multi
  |> String.Caseless.Map.to_alist
  |> List.map ~f:(fun (x, y) -> String.lowercase x, List.map y ~f:String.strip)
  |> List.sort ~compare:(fun (x1, _) (x2, _) -> String.compare x1 x2)
;;

let canonical_request ~http_method ~uri ~headers ~payload_hash =
  let headers = canonical_headers headers in
  match List.Assoc.mem headers "host" ~equal:String.equal with
  | false -> failwith "headers must include Host"
  | true ->
    [ Cohttp.Code.string_of_method http_method
    ; canonical_uri_path uri
    ; canonical_uri_query uri
    ; List.map headers ~f:(fun (x, y) -> sprintf "%s:%s\n" x (String.concat ~sep:"," y))
      |> String.concat ~sep:""
    ; List.map headers ~f:fst |> String.concat ~sep:";"
    ; Payload_header.to_value payload_hash
    ]
    |> String.concat ~sep:"\n"
    |> fun x -> x
;;

let credential_scope ~timestamp ~region ~service =
  String.concat
    ~sep:"/"
    [ Date_header.datestamp timestamp
    ; Region.to_string region
    ; service_to_string_no_api_prefix service
    ; "aws4_request"
    ]
;;

let string_to_sign ~canonical_request ~credential_scope ~timestamp =
  String.concat
    ~sep:"\n"
    [ algorithm
    ; Date_header.to_value timestamp
    ; credential_scope
    ; hex_encode (sha256 canonical_request)
    ]
;;

let credential_with_access_key ?aws_access_key_id ~credential_scope () =
  match aws_access_key_id with
  | Some a -> sprintf "%s/%s" a credential_scope
  | None -> credential_scope
;;

let headers_with_date_and_payload_hash ?session_token ~timestamp ~payload_hash headers =
  Date_header.add timestamp headers
  |> fun header ->
  (match session_token with
   | Some s -> Session_token_header.add s header
   | None -> header)
  |> Payload_header.add payload_hash
;;

let signature ?aws_secret_access_key ~string_to_sign ~timestamp ~region ~service () =
  "AWS4" ^ Option.value ~default:"" aws_secret_access_key
  |> fun key ->
  hmac_sha256 ~key ~data:(Date_header.datestamp timestamp)
  |> fun key ->
  hmac_sha256 ~key ~data:(Region.to_string region)
  |> fun key ->
  hmac_sha256 ~key ~data:(service_to_string_no_api_prefix service)
  |> fun key ->
  hmac_sha256 ~key ~data:"aws4_request"
  |> fun key -> hmac_sha256 ~key ~data:string_to_sign |> hex_encode
;;

let authorization_header ?aws_access_key_id ~signature ~credential_scope ~headers () =
  ( "Authorization"
  , sprintf
      "%s Credential=%s, SignedHeaders=%s, Signature=%s"
      algorithm
      (credential_with_access_key ?aws_access_key_id ~credential_scope ())
      (canonical_headers headers |> List.map ~f:fst |> String.concat ~sep:";")
      signature )
;;

(******************************************************************************)
(* Main API                                                                   *)
(******************************************************************************)
let sign_url
  ~http_method
  ~region
  ~service
  ~timestamp
  ~headers
  ?aws_secret_access_key
  ?aws_access_key_id
  ~(payload_hash : [ `Signed of payload_hash | `Unsigned ])
  ?timeout
  uri
  =
  (match timeout with
   | None -> timeout
   | Some x ->
     if x < 1 || x > 604800
     then
       failwiths
         ~here:[%here]
         "timeout must be between 1 and 604800 seconds"
         x
         sexp_of_int
     else timeout)
  |> fun timeout ->
  let credential_scope = credential_scope ~timestamp ~region ~service in
  let credential =
    match aws_access_key_id with
    | Some a -> sprintf "%s/%s" a credential_scope
    | None -> credential_scope
  in
  let header_names =
    canonical_headers headers |> List.map ~f:fst |> String.concat ~sep:";"
  in
  let uri =
    [ Some ("X-Amz-Algorithm", algorithm)
    ; Some ("X-Amz-Credential", credential)
    ; Some ("X-Amz-Date", Date_header.amzdate timestamp)
    ; Option.map timeout ~f:(fun x -> "X-Amz-Expires", Int.to_string x)
    ; Some ("X-Amz-SignedHeaders", header_names)
    ]
    |> List.filter_map ~f:Fn.id
    |> Uri.add_query_params' uri
  in
  canonical_request ~http_method ~uri ~headers ~payload_hash
  |> fun canonical_request ->
  let signature =
    let string_to_sign = string_to_sign ~canonical_request ~credential_scope ~timestamp in
    signature ~string_to_sign ?aws_secret_access_key ~timestamp ~region ~service ()
  in
  Uri.add_query_param' uri ("X-Amz-Signature", signature)
;;

let sign_request
  ?session_token
  ?aws_access_key_id
  ?aws_secret_access_key
  ~region
  ~service
  ~payload_hash
  req
  =
  let timestamp = Time.now () in
  let headers =
    headers_with_date_and_payload_hash
      ?session_token
      ~payload_hash:(`Signed payload_hash)
      ~timestamp
      req.Cohttp.Request.headers
  in
  let canonical_request_headers =
    (* 2022-05-03 mbac: I had to reduce the canonical request headers to this
       restricted set to get EC2 to work. The aws cli tool appears to do the same
       (run it with --debug) *)
    headers
    |> Cohttp.Header.to_list
    |> List.filter ~f:(fun (k, _) ->
         match String.lowercase k with
         | "host" | "x-amz-date" | "content-type" | "x-amz-security-token" -> true
         | _ -> false)
    |> Cohttp.Header.of_list
  in
  let canonical_request =
    canonical_request
      ~http_method:req.Cohttp.Request.meth
      ~uri:(Cohttp.Request.uri req)
      ~headers:canonical_request_headers
      ~payload_hash:(`Signed payload_hash)
  in
  (* printf "Canonical request:\n%s\n" canonical_request; *)
  let credential_scope_region =
    Botocore_endpoints.lookup_credential_scope ~region service
  in
  let credential_scope =
    credential_scope ~timestamp ~region:credential_scope_region ~service
  in
  let (signature : string) =
    let string_to_sign = string_to_sign ~canonical_request ~credential_scope ~timestamp in
    (* printf "String to sign:\n%s\n" string_to_sign; *)
    signature
      ~string_to_sign
      ?aws_secret_access_key
      ~timestamp
      ~region:credential_scope_region
      ~service
      ()
  in
  let auth_header_name, auth_header_val =
    authorization_header
      ~signature
      ~credential_scope
      ?aws_access_key_id
      ~headers:canonical_request_headers
      ()
  in
  { req with
    Cohttp.Request.headers = Cohttp.Header.add headers auth_header_name auth_header_val
  }
;;
