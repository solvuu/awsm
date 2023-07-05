open Awsm
open! Import

type s3_event_notification_event_name =
  [ `Object_created of [ `Star | `Put | `Post | `Copy | `Complete_multipart_upload ]
  | `Object_removed of [ `Star | `Delete | `Delete_marker_created ]
  | `Reduced_redundancy_lost_object
  | `Unknown of string
  ]
[@@deriving sexp_of]

type request_parameters_entity = { source_ip_address : Core_unix.Inet_addr.t }
[@@deriving sexp_of]

type response_elements_entity =
  { x_amz_id_2 : string
  ; x_amz_request_id : string
  }
[@@deriving sexp_of]

type user_identity_entity = { principal_id : string } [@@deriving sexp_of]

type s3_bucket_entity =
  { name : string
  ; owner_identity : user_identity_entity
  ; arn : string
  }
[@@deriving sexp_of]

type s3_object_entity =
  { key : string
  ; size : int64
  ; e_tag : string
  ; version_id : string option
  ; sequencer : string
  }
[@@deriving sexp_of]

type s3_entity =
  { configuration_id : string
  ; bucket : s3_bucket_entity
  ; object_ : s3_object_entity
  ; s3_schema_version : string
  }
[@@deriving sexp_of]

type s3_event_notification_record =
  { aws_region : Region.t
  ; event_name : s3_event_notification_event_name
  ; event_source : string
  ; event_time : Time_unix.t option
  ; event_version : string
  ; request_parameters : request_parameters_entity
  ; response_elements : response_elements_entity
  ; s3 : s3_entity
  ; user_identity : user_identity_entity
  }
[@@deriving sexp_of]

type s3_event_notification = { records : s3_event_notification_record list }
[@@deriving sexp_of]

type t = s3_event_notification [@@deriving sexp_of]

let request_parameters_entity_fields = [ "sourceIPAddress" ]
let response_elements_entity_fields = [ "x-amz-id-2"; "x-amz-request-id" ]
let user_identity_entity_fields = [ "principalId" ]
let s3_bucket_entity_fields = [ "name"; "ownerIdentity"; "arn" ]
let s3_object_entity_fields = [ "key"; "size"; "eTag"; "versionId"; "sequencer" ]
let s3_entity_fields = [ "configurationId"; "bucket"; "object"; "s3SchemaVersion" ]

let s3_event_notification_record_fields =
  [ "awsRegion"
  ; "eventName"
  ; "eventSource"
  ; "eventTime"
  ; "eventVersion"
  ; "requestParameters"
  ; "responseElements"
  ; "s3"
  ; "userIdentity"
  ]
;;

let s3_event_notification_fields = [ "Records" ]

type shape =
  [ `Request_parameters_entity
  | `Response_elements_entity
  | `User_identity_entity
  | `S3_bucket_entity
  | `S3_object_entity
  | `S3_entity
  | `S3_event_notification_record
  | `S3_event_notification
  ]

module Exn = struct
  exception Unexpected_fields of shape * string list

  module J = struct
    let find x f =
      match x with
      | `Assoc lst -> List.find_exn lst ~f:(fun (k, _v) -> String.equal k f) |> snd
      | _ -> failwithf "Expected object for find  %s" f ()
    ;;

    let get_string v = Yojson.Safe.Util.to_string v
    let get_list v = Yojson.Safe.Util.to_list v

    let get_int64 v =
      match v with
      | `Int i -> Int64.of_int i
      | `Intlit s -> Int64.of_string s
      | _ -> failwith "get_int64 failed"
    ;;

    let field x fld =
      try find x fld with
      | Not_found_s _ -> failwithf "No field named %s" fld ()
    ;;

    let opt_field x fld = Option.try_with (fun () -> find x fld)

    let string_field x fld =
      field x fld
      |> fun v ->
      try get_string v with
      | Yojson.Safe.Util.Type_error (msg, _) ->
        failwithf "Field %s is not a string: %s" fld msg ()
    ;;

    let opt_string_field x fld =
      match opt_field x fld with
      | Some v -> (
        try Some (get_string v) with
        | Yojson.Safe.Util.Type_error (msg, _) ->
          failwithf "Field %s is not a string: %s" fld msg ())
      | None -> None
    ;;
  end

  let check_fields ~shape fields = function
    | `Assoc values -> (
      match
        List.filter values ~f:(fun (k, _) -> not (List.mem fields k ~equal:String.equal))
      with
      | [] -> ()
      | unexpected -> raise (Unexpected_fields (shape, List.map unexpected ~f:fst)))
    | _ -> failwith "Expected object value"
  ;;

  let parse_s3_event_notification_record_event_name = function
    | "ObjectCreated:*" -> `Object_created `Star
    | "ObjectCreated:Put" -> `Object_created `Put
    | "ObjectCreated:Post" -> `Object_created `Post
    | "ObjectCreated:Copy" -> `Object_created `Copy
    | "ObjectCreated:CompleteMultipartUpload" ->
      `Object_created `Complete_multipart_upload
    | "ObjectRemoved:*" -> `Object_removed `Star
    | "ObjectRemoved:Delete" -> `Object_removed `Delete
    | "ObjectRemoved:DeleteMarkerCreated" -> `Object_removed `Delete_marker_created
    | "ReducedRedundancyLostObject" -> `Reduced_redundancy_lost_object
    | unknown -> `Unknown unknown
  ;;

  let parse_request_parameters_entity (x : Yojson.Safe.t) =
    check_fields ~shape:`Request_parameters_entity request_parameters_entity_fields x
    |> fun () ->
    J.string_field x "sourceIPAddress"
    |> Core_unix.Inet_addr.of_string
    |> fun source_ip_address : request_parameters_entity -> { source_ip_address }
  ;;

  let parse_response_elements_entity x =
    check_fields ~shape:`Response_elements_entity response_elements_entity_fields x
    |> fun () ->
    J.string_field x "x-amz-id-2"
    |> fun x_amz_id_2 ->
    J.string_field x "x-amz-request-id"
    |> fun x_amz_request_id : response_elements_entity -> { x_amz_id_2; x_amz_request_id }
  ;;

  let parse_user_identity_entity x =
    check_fields ~shape:`User_identity_entity user_identity_entity_fields x
    |> fun () ->
    J.string_field x "principalId"
    |> fun principal_id : user_identity_entity -> { principal_id }
  ;;

  let parse_s3_bucket_entity x =
    check_fields ~shape:`S3_bucket_entity s3_bucket_entity_fields x
    |> fun () ->
    J.string_field x "name"
    |> fun name ->
    J.field x "ownerIdentity"
    |> parse_user_identity_entity
    |> fun owner_identity ->
    J.string_field x "arn" |> fun arn : s3_bucket_entity -> { name; owner_identity; arn }
  ;;

  let parse_s3_object_entity x =
    check_fields ~shape:`S3_object_entity s3_object_entity_fields x
    |> fun () ->
    J.string_field x "key"
    |> fun key ->
    J.field x "size"
    |> J.get_int64
    |> fun size ->
    J.string_field x "eTag"
    |> fun e_tag ->
    J.opt_string_field x "versionId"
    |> fun version_id ->
    J.string_field x "sequencer"
    |> fun sequencer : s3_object_entity -> { key; size; e_tag; version_id; sequencer }
  ;;

  let parse_s3_entity x =
    check_fields ~shape:`S3_entity s3_entity_fields x
    |> fun () ->
    J.string_field x "configurationId"
    |> fun configuration_id ->
    J.field x "bucket"
    |> parse_s3_bucket_entity
    |> fun bucket ->
    J.field x "object"
    |> parse_s3_object_entity
    |> fun object_ ->
    J.string_field x "s3SchemaVersion"
    |> fun s3_schema_version : s3_entity ->
    { configuration_id; bucket; object_; s3_schema_version }
  ;;

  let parse_s3_event_notification_record x =
    check_fields
      ~shape:`S3_event_notification_record
      s3_event_notification_record_fields
      x
    |> fun () ->
    J.string_field x "awsRegion"
    |> Region.of_string
    |> fun aws_region ->
    J.string_field x "eventName"
    |> parse_s3_event_notification_record_event_name
    |> fun event_name ->
    J.string_field x "eventSource"
    |> fun event_source ->
    J.opt_string_field x "eventTime"
    |> Option.map ~f:Time_unix.of_string
    |> fun event_time ->
    J.string_field x "eventVersion"
    |> fun event_version ->
    J.field x "requestParameters"
    |> parse_request_parameters_entity
    |> fun request_parameters ->
    J.field x "responseElements"
    |> parse_response_elements_entity
    |> fun response_elements ->
    J.field x "s3"
    |> parse_s3_entity
    |> fun s3 ->
    J.field x "userIdentity"
    |> parse_user_identity_entity
    |> fun user_identity : s3_event_notification_record ->
    { aws_region
    ; event_name
    ; event_source
    ; event_time
    ; event_version
    ; request_parameters
    ; response_elements
    ; s3
    ; user_identity
    }
  ;;

  let parse_s3_event_notification x =
    check_fields ~shape:`S3_event_notification s3_event_notification_fields x
    |> fun () ->
    J.field x "Records"
    |> J.get_list
    |> List.map ~f:parse_s3_event_notification_record
    |> fun records : s3_event_notification -> { records }
  ;;
end

let of_json x =
  try `Ok (Exn.parse_s3_event_notification x : t) with
  | Exn.Unexpected_fields (name, fields) -> `Unexpected_fields (name, fields)
  | Yojson.Safe.Util.Type_error (msg, _) -> `Parse_error msg
;;

let of_string x =
  try Yojson.Safe.from_string x |> of_json with
  | Yojson.Json_error msg -> `Parse_error msg
;;
