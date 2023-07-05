open! Core
open! Import

let to_ocaml x =
  x
  |> String.substr_replace_all ~pattern:"-" ~with_:"_"
  |> ( ^ ) "Service."
  |> Ast_convenience.evar
;;

module TimestampFormat = struct
  type t = Botodata.timestampFormat

  let parser =
    let open Json_parser in
    map_result string ~f:(function
      | "unixTimestamp" -> Ok `unixTimestamp
      | "rfc822" -> Ok `rfc822
      | "iso8601" -> Ok `iso8601
      | x -> Result.failf "Unknown timestampFormat %s" x)
  ;;
end

module Location = struct
  type t = Botodata.location

  let parser =
    let open Json_parser in
    map_result string ~f:(function
      | "header" -> Ok `header
      | "headers" -> Ok `headers
      | "uri" -> Ok `uri
      | "querystring" -> Ok `querystring
      | "statusCode" -> Ok `statusCode
      | x -> Result.failf "Unknown location %s" x)
  ;;
end

module Uri = struct
  type t = Uri.t

  let parser =
    let open Json_parser in
    string >>| Uri.of_string
  ;;
end

module Metadata = struct
  module ChecksumFormat = struct
    type t = Botodata.checksumFormat

    let parser =
      let open Json_parser in
      map_result string ~f:(function
        | "md5" -> Ok `md5
        | "sha256" -> Ok `sha256
        | x -> Result.failf "Unknown checksumFormat %s" x)
    ;;
  end

  module Protocol = struct
    type t = Botodata.protocol

    let parser =
      let open Json_parser in
      map_result string ~f:(function
        | "query" -> Ok `query
        | "json" -> Ok `json
        | "rest-json" -> Ok `rest_json
        | "rest-xml" -> Ok `rest_xml
        | "ec2" -> Ok `ec2
        | x -> Result.failf "Unknown protocol %s" x)
    ;;
  end

  type t = Botodata.metadata

  let parser =
    let open Json_parser in
    record
      (let%map apiVersion = field "apiVersion" string
       and checksumFormat = field_opt "checksumFormat" ChecksumFormat.parser
       and endpointPrefix = field "endpointPrefix" string
       and globalEndpoint = field_opt "globalEndpoint" Uri.parser
       and serviceAbbreviation = field_opt "serviceAbbreviation" string
       and serviceFullName = field "serviceFullName" string
       and serviceId = field_opt "serviceId" string
       and signatureVersion = field "signatureVersion" string
       and timestampFormat = field_opt "timestampFormat" TimestampFormat.parser
       and protocol = field "protocol" Protocol.parser
       and () = field_ignored "protocolSettings"
       and jsonVersion = field_opt "jsonVersion" string
       and targetPrefix = field_opt "targetPrefix" Uri.parser
       and xmlNamespace = field_opt "xmlNamespace" Uri.parser
       and signingName = field_opt "signingName" string
       and uid = field_opt "uid" string in
       { Botodata.apiVersion
       ; checksumFormat
       ; endpointPrefix
       ; globalEndpoint
       ; serviceAbbreviation
       ; serviceFullName
       ; serviceId
       ; signatureVersion
       ; timestampFormat
       ; protocol
       ; jsonVersion
       ; targetPrefix
       ; signingName
       ; xmlNamespace
       ; uid
       })
  ;;
end

let parse_requestUri x : Botodata.requestUri =
  let requestUri_path_token buf =
    let open Sedlexing.Latin1 in
    match%sedlex buf with
    | '/' -> Ok (Some `Slash)
    | '{' -> Ok (Some `Lbrace)
    | '}' -> Ok (Some `Rbrace)
    | '+' -> Ok (Some `Plus)
    | '&' -> Ok (Some `Ampersand)
    | '=' -> Ok (Some `Equal)
    | '?' -> Ok (Some `Qmark)
    | eof -> Ok None
    | Plus (Compl (Chars "?/{}+&=")) -> Ok (Some (`String (lexeme buf)))
    | _ -> Error `Invalid
  in
  let rec parse = function
    | [] -> []
    | `Slash :: t -> `Slash :: parse t
    | `Lbrace :: `String s :: `Rbrace :: t -> `Variable (s, false) :: parse t
    | `Lbrace :: `String s :: `Plus :: `Rbrace :: t -> `Variable (s, true) :: parse t
    | `String s :: t -> `String s :: parse t
    | `Qmark :: t -> `Qmark :: parse t
    | `Ampersand :: t -> `Ampersand :: parse t
    | `Equal :: t -> `Equal :: parse t
    | _ ->
      (* The error string is not relevant. The caller below immediately discards it. *)
      failwith "malformed"
  in
  match Util.tokenize requestUri_path_token x with
  | Error `Invalid -> failwithf "Invalid requestUri: %s" x ()
  | Ok y -> (
    try parse y with
    | Failure _ -> failwithf "Invalid requestUri: %s" x ())
;;

module XmlNamespace = struct
  type t = Botodata.xmlNamespace

  let parser =
    let open Json_parser in
    record
      (let%map uri = field "uri" Uri.parser
       and prefix = field_opt "prefix" string in
       { Botodata.uri; prefix })
  ;;
end

module Error = struct
  type t = Botodata.error

  let parser =
    let open Json_parser in
    record
      (let%map code = field_opt "code" string
       and httpStatusCode = field "httpStatusCode" int
       and senderFault = field_opt "senderFault" bool in
       { Botodata.code; httpStatusCode; senderFault })
  ;;
end

let parse_operation_error_aux =
  let open Json_parser in
  let%map shape = field "shape" string
  and documentation = field_opt "documentation" string
  and exception_ = field_opt "exception" bool
  and fault = field_opt "fault" bool
  and error = field_opt "error" Error.parser
  and xmlOrder = field_opt "xmlOrder" (list string) in
  { Botodata.shape; documentation; exception_; fault; error; xmlOrder }
;;

module Operation = struct
  module Input = struct
    type t = Botodata.operation_input

    let parser =
      let open Json_parser in
      record
        (let%map shape = field "shape" string
         and documentation = field_opt "documentation" string
         and deprecated = field_opt "deprecated" bool
         and xmlNamespace = field_opt "xmlNamespace" XmlNamespace.parser
         and locationName = field_opt "locationName" string in
         { Botodata.shape; documentation; deprecated; xmlNamespace; locationName })
    ;;
  end

  module Output = struct
    type t = Botodata.operation_output

    let parser =
      let open Json_parser in
      record
        (let%map shape = field "shape" string
         and documentation = field_opt "documentation" string
         and deprecated = field_opt "deprecated" bool
         and locationName = field_opt "locationName" string
         and resultWrapper = field_opt "resultWrapper" string
         and wrapper = field_opt "wrapper" bool
         and xmlOrder = field_opt "xmlOrder" (list string) in
         { Botodata.shape
         ; documentation
         ; deprecated
         ; locationName
         ; resultWrapper
         ; wrapper
         ; xmlOrder
         })
    ;;
  end

  module Error = struct
    type t = Botodata.operation_error

    let parser = Json_parser.record_or_list_of parse_operation_error_aux
  end

  module Endpoint = struct
    type t = Botodata.operation_endpoint

    let parser =
      let open Json_parser in
      record
        (let%map hostPrefix = field "hostPrefix" string in
         { Botodata.hostPrefix })
    ;;
  end

  module Endpointdiscovergy = struct
    type t = Botodata.operation_endpointdiscovery

    let parser =
      let open Json_parser in
      record
        (let%map required = field_opt "required" bool in
         { Botodata.required })
    ;;
  end

  module Http = struct
    type t = Botodata.http

    let parser =
      let open Json_parser in
      record
        (let%map method_ =
           field "method" (map_result string ~f:Botodata.http_method_of_string)
         and requestUri = field "requestUri" (string >>| parse_requestUri)
         and responseCode = field_opt "responseCode" int in
         { Botodata.method_; requestUri; responseCode })
    ;;
  end

  module HttpChecksum = struct
    type t = Botodata.httpChecksum

    let parser =
      let open Json_parser in
      record
        (let%map requestValidationModeMember =
           field_opt "requestValidationModeMember" string
         and requestAlgorithmMember = field_opt "requestAlgorithmMember" string
         and requestChecksumRequired = field_opt "requestChecksumRequired" bool
         and responseAlgorithms = field_opt "responseAlgorithms" (list string) in
         { Botodata.requestValidationModeMember
         ; requestAlgorithmMember
         ; requestChecksumRequired
         ; responseAlgorithms
         })
    ;;
  end

  type t = Botodata.operation

  let parser =
    let open Json_parser in
    record
      (let%map name = field "name" string
       and http = field "http" Http.parser
       and input = field_opt "input" Input.parser
       and output = field_opt "output" Output.parser
       and errors = field_opt "errors" Error.parser
       and documentation = field_opt "documentation" string
       and documentationUrl = field_opt "documentationUrl" Uri.parser
       and alias = field_opt "alias" string
       and deprecated = field_opt "deprecated" bool
       and deprecatedMessage = field_opt "deprecatedMessage" string
       and authtype = field_opt "authtype" string
       and idempotent = field_opt "idempotent" bool
       and httpChecksum = field_opt "httpChecksum" HttpChecksum.parser
       and endpoint = field_opt "endpoint" Endpoint.parser
       and endpointdiscovery = field_opt "endpointdiscovery" Endpointdiscovergy.parser
       and _endpointoperation = field_opt "endpointoperation" bool in
       { Botodata.name
       ; http
       ; input
       ; output
       ; errors
       ; documentation
       ; documentationUrl
       ; alias
       ; deprecated
       ; deprecatedMessage
       ; authtype
       ; idempotent
       ; httpChecksum
       ; endpoint
       ; endpointdiscovery
       })
  ;;
end

module Operations = struct
  type t = Operation.t list

  let parser =
    let open Json_parser in
    dict Operation.parser >>| List.map ~f:snd
  ;;
end

module Shape = struct
  module Bool = struct
    let parser =
      let open Json_parser in
      let%map box = field_opt "box" bool
      and documentation = field_opt "documentation" string in
      Botodata.Boolean_shape { box; documentation }
    ;;
  end

  module Blob = struct
    let parser =
      let open Json_parser in
      let%map streaming = field_opt "streaming" bool
      and sensitive = field_opt "sensitive" bool
      and min = field_opt "min" int
      and max = field_opt "max" int
      and documentation = field_opt "documentation" string in
      Botodata.Blob_shape { streaming; sensitive; min; max; documentation }
    ;;
  end

  module Long = struct
    let parser =
      let open Json_parser in
      let%map box = field_opt "box" bool
      and min = field_opt "min" int64
      and max = field_opt "max" int64
      and documentation = field_opt "documentation" string in
      Botodata.Long_shape { box; min; max; documentation }
    ;;
  end

  module Double = struct
    let parser =
      let open Json_parser in
      let%map box = field_opt "box" bool
      and documentation = field_opt "documentation" string
      and min = field_opt "min" float
      and max = field_opt "max" float in
      Botodata.Double_shape { box; documentation; min; max }
    ;;
  end

  module Float = struct
    let parser =
      let open Json_parser in
      let%map box = field_opt "box" bool
      and documentation = field_opt "documentation" string
      and min = field_opt "min" float
      and max = field_opt "max" float in
      Botodata.Float_shape { box; documentation; min; max }
    ;;
  end

  module Integer = struct
    let parser =
      let open Json_parser in
      let%map min = field_opt "min" int
      and max = field_opt "max" int
      and documentation = field_opt "documentation" string
      and box = field_opt "box" bool
      and deprecated = field_opt "deprecated" bool
      and deprecatedMessage = field_opt "deprecatedMessage" string in
      Botodata.Integer_shape
        { min; max; documentation; box; deprecated; deprecatedMessage }
    ;;
  end

  module Timestamp = struct
    let parser =
      let open Json_parser in
      let%map timestampFormat = field_opt "timestampFormat" TimestampFormat.parser
      and documentation = field_opt "documentation" string in
      Botodata.Timestamp_shape { timestampFormat; documentation }
    ;;
  end

  module Member = struct
    type t = Botodata.shape_member

    let parser =
      let open Json_parser in
      record
        (let%map shape = field "shape" string
         and deprecated = field_opt "deprecated" bool
         and deprecatedMessage = field_opt "deprecatedMessage" string
         and location = field_opt "location" Location.parser
         and locationName = field_opt "locationName" string
         and documentation = field_opt "documentation" string
         and xmlNamespace = field_opt "xmlNamespace" XmlNamespace.parser
         and streaming = field_opt "streaming" bool
         and xmlAttribute = field_opt "xmlAttribute" bool
         and queryName = field_opt "queryName" string
         and box = field_opt "box" bool
         and flattened = field_opt "flattened" bool
         and idempotencyToken = field_opt "idempotencyToken" bool
         and eventpayload = field_opt "eventpayload" bool
         and hostLabel = field_opt "hostLabel" bool
         and jsonvalue = field_opt "jsonvalue" bool in
         { Botodata.shape
         ; deprecated
         ; deprecatedMessage
         ; location
         ; locationName
         ; documentation
         ; xmlNamespace
         ; streaming
         ; xmlAttribute
         ; queryName
         ; box
         ; flattened
         ; idempotencyToken
         ; eventpayload
         ; hostLabel
         ; jsonvalue
         })
    ;;
  end

  module List = struct
    let parser =
      let open Json_parser in
      let%map member = field "member" Member.parser
      and min = field_opt "min" int
      and max = field_opt "max" int
      and documentation = field_opt "documentation" string
      and flattened = field_opt "flattened" bool
      and () = field_ignored "locationName"
      and sensitive = field_opt "sensitive" bool
      and deprecated = field_opt "deprecated" bool
      and deprecatedMessage = field_opt "deprecatedMessage" string in
      Botodata.List_shape
        { member
        ; min
        ; max
        ; documentation
        ; flattened
        ; sensitive
        ; deprecatedMessage
        ; deprecated
        }
    ;;
  end

  module Map = struct
    let parser =
      let open Json_parser in
      let shape =
        record
          (let%map shape = field "shape" string
           and () = field_ignored "locationName"
           and () = field_ignored "documentation" in
           shape)
      in
      let%map key = field "key" shape
      and value = field "value" shape
      and min = field_opt "min" int
      and max = field_opt "max" int
      and flattened = field_opt "flattened" bool
      and locationName = field_opt "locationName" string
      and documentation = field_opt "documentation" string
      and sensitive = field_opt "sensitive" bool in
      Botodata.Map_shape
        { key; value; min; max; flattened; locationName; documentation; sensitive }
    ;;
  end

  module Retryable = struct
    type t = Botodata.retryable

    let parser =
      let open Json_parser in
      record
        (let%map throttling = field "throttling" bool in
         { Botodata.throttling })
    ;;
  end

  module Structure = struct
    let parser =
      let open Json_parser in
      let%map required = field_opt "required" (list string)
      and members = field "members" (dict Member.parser)
      and error = field_opt "error" Error.parser
      and exception_ = field_opt "exception" bool
      and fault = field_opt "fault" bool
      and wrapper = field_opt "wrapper" bool
      and deprecated = field_opt "deprecated" bool
      and deprecatedMessage = field_opt "deprecatedMessage" string
      and sensitive = field_opt "sensitive" bool
      and documentation = field_opt "documentation" string
      and document = field_opt "document" bool
      and payload = field_opt "payload" string
      and xmlNamespace = field_opt "xmlNamespace" XmlNamespace.parser
      and xmlOrder = field_opt "xmlOrder" (list string)
      and locationName = field_opt "locationName" string
      and event = field_opt "event" bool
      and eventstream = field_opt "eventstream" bool
      and _synthetic = field_opt "synthetic" bool
      and retryable = field_opt "retryable" Retryable.parser
      and union = field_opt "union" bool
      and box = field_opt "box" bool in
      Botodata.Structure_shape
        { required
        ; members
        ; error
        ; exception_
        ; fault
        ; documentation
        ; document
        ; payload
        ; xmlNamespace
        ; xmlOrder
        ; wrapper
        ; deprecated
        ; deprecatedMessage
        ; sensitive
        ; locationName
        ; event
        ; eventstream
        ; retryable
        ; union
        ; box
        }
    ;;
  end

  module Enum = struct
    let parser =
      let open Json_parser in
      let%map cases = field "enum" (list string)
      and documentation = field_opt "documentation" string
      and min = field_opt "min" int
      and max = field_opt "max" int
      and pattern = field_opt "pattern" string
      and deprecated = field_opt "deprecated" bool
      and deprecatedMessage = field_opt "deprecatedMessage" string
      and sensitive = field_opt "sensitive" bool in
      Botodata.Enum_shape
        { cases
        ; documentation
        ; min
        ; max
        ; pattern
        ; deprecated
        ; deprecatedMessage
        ; sensitive
        }
    ;;
  end

  module String = struct
    let parser =
      let open Json_parser in
      let%map pattern = field_opt "pattern" string
      and min = field_opt "min" int
      and max = field_opt "max" int
      and sensitive = field_opt "sensitive" bool
      and documentation = field_opt "documentation" string
      and deprecated = field_opt "deprecated" bool
      and deprecatedMessage = field_opt "deprecatedMessage" string in
      Botodata.String_shape
        { pattern; min; max; sensitive; documentation; deprecated; deprecatedMessage }
    ;;
  end

  type t = Botodata.shape

  let parser =
    let open Json_parser in
    field_based "type" (function
      | "boolean" -> Some Bool.parser
      | "integer" -> Some Integer.parser
      | "blob" -> Some Blob.parser
      | "long" -> Some Long.parser
      | "double" -> Some Double.parser
      | "float" -> Some Float.parser
      | "timestamp" -> Some Timestamp.parser
      | "structure" -> Some Structure.parser
      | "list" -> Some List.parser
      | "map" -> Some Map.parser
      | "string" -> Some (if_field_present "enum" ~then_:Enum.parser ~else_:String.parser)
      | _ -> None)
  ;;
end

module Shapes = struct
  type t = (string * Botodata.shape) list

  let parser = Json_parser.dict Shape.parser
end

type t = Botodata.service

let parse =
  let open Json_parser in
  Json_parser.run_exn
    (record
       (let%map metadata = field "metadata" Metadata.parser
        and documentation = field_opt "documentation" string
        and shapes = field "shapes" Shapes.parser
        and operations = field "operations" Operations.parser
        and version = field_opt "version" string
        and () = field_ignored "examples" in
        { Botodata.metadata; version; operations; shapes; documentation }))
;;

let fix_shape_name_collisions (service : Botodata.service) =
  let begins_lowercase s = Int.( > ) (String.length s) 0 && Char.is_lowercase s.[0] in
  let seen =
    service.shapes
    |> List.fold ~init:String.Set.empty ~f:(fun set (name, _shape) ->
         if begins_lowercase name then set else String.Set.add set name)
  in
  let fixups = String.Table.create () in
  let collides_when_uppercased name = String.Set.mem seen (String.capitalize name) in
  let fixup_needed name = begins_lowercase name && collides_when_uppercased name in
  List.iter service.shapes ~f:(fun (name, _shape) ->
    if fixup_needed name
    then (
      let fixed_name = String.capitalize name ^ "__lc1" in
      String.Table.add_exn fixups ~key:name ~data:fixed_name));
  let maybe_get_fixed_name name =
    String.Table.find fixups name |> Option.value ~default:name
  in
  (* rewrite service with names fixed up *)
  { service with
    shapes =
      List.map service.shapes ~f:(fun (name, shape) ->
        let name = maybe_get_fixed_name name in
        let shape =
          match shape with
          | String_shape _
          | Integer_shape _
          | Long_shape _
          | Boolean_shape _
          | Float_shape _
          | Double_shape _
          | Blob_shape _
          | Timestamp_shape _
          | List_shape _
          | Enum_shape _ -> shape
          | Map_shape ms ->
            Map_shape
              { ms with
                key = maybe_get_fixed_name ms.key
              ; value = maybe_get_fixed_name ms.value
              }
          | Structure_shape ss ->
            Structure_shape
              { ss with
                members =
                  List.map ss.members ~f:(fun (name, sm) ->
                    let name = maybe_get_fixed_name name in
                    let shape_member =
                      { sm with shape = maybe_get_fixed_name sm.shape }
                    in
                    name, shape_member)
              }
        in
        name, shape)
  ; operations =
      List.map service.operations ~f:(fun operation ->
        { operation with
          input =
            Option.map operation.input ~f:(fun input ->
              { input with shape = maybe_get_fixed_name input.shape })
        ; output =
            Option.map operation.output ~f:(fun output ->
              { output with shape = maybe_get_fixed_name output.shape })
        ; errors =
            Option.map operation.errors ~f:(fun errors ->
              List.map errors ~f:(fun error ->
                { error with shape = maybe_get_fixed_name error.shape }))
        })
  }
;;

let of_json x = x |> Json.from_string |> parse |> fix_shape_name_collisions
