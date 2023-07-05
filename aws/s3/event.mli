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
  ; event_time : Time.t option
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

val of_json
  :  Yojson.Safe.t
  -> [ `Ok of t | `Parse_error of string | `Unexpected_fields of shape * string list ]

val of_string
  :  string
  -> [ `Ok of t | `Parse_error of string | `Unexpected_fields of shape * string list ]
