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

module Json0 = struct
  let uri_token buf : (uri_token option, [ `Invalid ]) result =
    match%sedlex buf with
    | "{dnsSuffix}" -> Ok (Some `DnsSuffix_token)
    | "{service}" -> Ok (Some `Service_token)
    | "{region}" -> Ok (Some `Region_token)
    | eof -> Ok None
    | Plus (Compl (Chars "{}")) -> Ok (Some (`String_token (Sedlexing.Latin1.lexeme buf)))
    | _ -> Error `Invalid
  ;;

  let parse_uri s =
    match Util.tokenize uri_token s with
    | Ok _ as x -> x
    | Error `Invalid -> Result.failf "Invalid uri: %s" s
  ;;

  (*
let constraint_ =
  Json_parser.parse_with (function
      | `List [ `String "region"; `String op; values ] ->
        let open Result.Let_syntax in
        let%bind op = parse_op op in
        let%bind values = parse_constraint_value values in
        let%map test =
          match op, values with
          | `equals, [ x ] -> Ok (`equals x)
          | `notEquals, [ x ] -> Ok (`notEquals x)
          | `startsWith, [ x ] -> Ok (`startsWith x)
          | `notStartsWith, [ x ] -> Ok (`notStartsWith x)
          | `oneOf, vals -> Ok (`oneOf vals)
          | _ -> Error "Malformed constraint"
        in
        Constraint (`Region, test)
      | `List _ -> Error "Expected array of size 3 starting with 'region' for constraint"
      | _ -> Error "Expected array for constraint")
;;
*)
  let parse_signatureVersion = function
    | "v2" -> Ok `v2
    | "v3" -> Ok `v3
    | "v4" -> Ok `v4
    | "s3" -> Ok `s3
    | "s3v4" -> Ok `s3v4
    | s -> Result.failf "Unknown signatureVersion %s" s
  ;;

  let credential_scope =
    let open Json_parser in
    record
      (let%map region = field_opt "region" (string >>| Region.of_string)
       and service = field_opt "service" string in
       { region; service })
  ;;

  let variant =
    let open Json_parser in
    record
      (let%map dnsSuffix = field_opt "dnsSuffix" string
       and hostname = field_opt "hostname" (map_result string ~f:parse_uri)
       and tags = field "tags" (list string) in
       { dnsSuffix; hostname; tags })
  ;;

  let properties =
    let open Json_parser in
    record
      (let%map signatureVersions =
         field_opt
           "signatureVersions"
           (list (map_result string ~f:parse_signatureVersion))
       and credentialScope = field_opt "credentialScope" credential_scope
       and hostname = field_opt "hostname" (map_result string ~f:parse_uri)
       and protocols = field_opt "protocols" (list string)
       and sslCommonName = field_opt "sslCommonName" string
       and variants = field_opt "variants" (list variant)
       and deprecated = field_opt "deprecated" bool in
       { credentialScope
       ; hostname
       ; protocols
       ; sslCommonName
       ; signatureVersions
       ; variants
       ; deprecated
       })
  ;;

  let region =
    let open Json_parser in
    record
      (let%map description = field "description" string in
       { description })
  ;;

  let endpoint_service =
    let open Json_parser in
    record
      (let%map defaults = field_opt "defaults" properties
       and endpoints = field "endpoints" (dict properties)
       and isRegionalized = field_opt "isRegionalized" bool
       and partitionEndpoint = field_opt "partitionEndpoint" string in
       { defaults; endpoints; isRegionalized; partitionEndpoint })
  ;;

  let partition =
    let open Json_parser in
    record
      (let%map defaults = field_opt "defaults" properties
       and dnsSuffix = field "dnsSuffix" string
       and partition = field "partition" string
       and partitionName = field "partitionName" string
       and regionRegex = field "regionRegex" string
       and regions = field "regions" (dict region)
       and services = field "services" (dict endpoint_service) in
       { defaults; dnsSuffix; partition; partitionName; regionRegex; regions; services })
  ;;
end

let of_json x =
  let t =
    x
    |> Json.from_string
    |> Json_parser.run_exn
         (let open Json_parser in
         record
           (let%map partitions = field "partitions" (list Json0.partition)
            and version = field "version" int in
            { partitions; version }))
  in
  let () =
    match t.version with
    | 3 -> ()
    | _ -> failwithf "unexpected version: %d" t.version ()
  in
  t
;;

module Endpoint_rules_for_precompute = struct
  module Botodata = Botodata

  let str_regexp = Memo.general Re.Perl.compile_pat

  let lookup_partition =
    Memo.general (fun (ep, region) ->
      let region_s = Region.to_string region in
      match
        List.find ep.partitions ~f:(fun partition ->
          let rex = str_regexp partition.regionRegex in
          Option.is_some (Re.exec_opt rex region_s))
      with
      | None -> failwithf !"no partition found for region: %{Region}" region ()
      | Some partition -> partition)
  ;;

  let lookup_service_properties_memo =
    Memo.general (fun (region, service, (partition : partition)) ->
      let region_s = Region.to_string region in
      let service_s = Service.to_string service in
      let service_s =
        (* FIXME: need to do more sophisticated Endpoint_rules processing.
       This is a temporary shim to get a critical service running. *)
        match service_s with
        | "sso" -> "portal.sso"
        | s -> s
      in
      match
        List.find partition.services ~f:(fun (service_name, _service_spec) ->
          String.( = ) service_name service_s)
      with
      | None -> None, None
      | Some (_service_name, service_spec) -> (
        let service_defaults = service_spec.defaults in
        let match_endpoint =
          match service_spec.isRegionalized with
          | None | Some false -> "aws-global"
          | Some true -> region_s
        in
        match
          List.find service_spec.endpoints ~f:(fun (endpoint, _properties) ->
            String.( = ) match_endpoint endpoint)
        with
        | None -> service_defaults, None
        | Some (_endpoint, properties) -> service_defaults, Some properties))
  ;;

  let lookup_service_properties ~region ~service ~partition =
    lookup_service_properties_memo (region, service, partition)
  ;;

  let lookup_credential_scope ep ~region:orig_region service =
    let partition = lookup_partition (ep, orig_region) in
    let credential_scope =
      match lookup_service_properties ~partition ~service ~region:orig_region with
      | _, None -> None
      | _, Some properties -> properties.credentialScope
    in
    (match credential_scope with
     | None -> orig_region
     | Some { region = None; service = _ } -> orig_region
     | Some { region = Some region; service = _ } -> region)
    |> Region.to_string
  ;;

  let lookup_uri ep ~scheme ~region ~service =
    let service_s = Service.to_string service in
    let service_s =
      (* FIXME: need to do more sophisticated Endpoint_rules processing.
       This is a temporary shim to get a critical service running. *)
      match service_s with
      | "sso" -> "portal.sso"
      | s -> s
    in
    let partition = lookup_partition (ep, region) in
    let partition_hostname =
      let partition_defaults =
        Option.value_exn
          partition.defaults
          ~message:(sprintf "no defaults for %s" partition.partitionName)
      in
      match partition_defaults.hostname with
      | None | Some [] ->
        failwithf "no default hostname schema for partition %s" partition.partitionName ()
      | Some hostname -> Some hostname
    in
    let service_hostname =
      match lookup_service_properties ~partition ~service ~region with
      | Some service_defaults, None -> service_defaults.hostname
      | Some _, Some { hostname = Some hostname; _ } -> Some hostname
      | Some service_defaults, Some { hostname = None; _ } -> service_defaults.hostname
      | None, Some { hostname = Some hostname; _ } -> Some hostname
      | None, Some { hostname = None; _ } -> None
      | None, None -> None
    in
    let scheme =
      match scheme with
      | `HTTP -> "http"
      | `HTTPS -> "https"
    in
    let hostname =
      match service_hostname, partition_hostname with
      | Some h, _ -> h
      | None, Some h -> h
      | _, None -> assert false
    in
    let host =
      List.map hostname ~f:(function
        | `String_token s -> s
        | `Service_token -> service_s
        | `Region_token -> Region.to_string region
        | `DnsSuffix_token -> partition.dnsSuffix)
    in
    String.concat (scheme :: "://" :: host) ~sep:""
  ;;
end

let gen_catch_all expr = Ast_helper.Exp.case Ast_convenience.(pvar "_unknown") expr

let make_lookup_uri ep =
  let loc = !Ast_helper.default_loc in
  let uri_rules =
    List.concat_map [ `HTTP; `HTTPS ] ~f:(fun scheme ->
      List.concat_map Region.all ~f:(fun region ->
        List.concat_map Service.all ~f:(fun service ->
          let uri =
            Endpoint_rules_for_precompute.lookup_uri ep ~scheme ~region ~service
          in
          [ scheme, region, service, uri ])))
  in
  let uri_cases =
    List.map uri_rules ~f:(fun (scheme, region, service, uri) ->
      let scheme_pvar =
        match scheme with
        | `HTTP -> "`HTTP"
        | `HTTPS -> "`HTTPS"
      in
      let region_s = Region.to_string region in
      let service_s = Service.to_string service in
      Ast_helper.Exp.case
        Ast_convenience.(ptuple [ pvar scheme_pvar; pstr region_s; pstr service_s ])
        (Ast_convenience.str uri))
    @ [ gen_catch_all
          [%expr
            failwithf
              "unknown endpoint for %s %s, %s"
              (match scheme with
               | `HTTPS -> "https"
               | `HTTP -> "http")
              region
              service
              ()]
      ]
  in
  let uri_match_with = Ast_helper.Exp.match_ [%expr scheme, region, service] uri_cases in
  [%stri
    let lookup_uri ~region service scheme =
      let region = Region.to_string region in
      let service = Service.to_string service in
      let uri_s = [%e uri_match_with] in
      Uri.of_string uri_s
    ;;]
;;

let make_lookup_credential_scope ep =
  let loc = !Ast_helper.default_loc in
  let credential_scope_rules =
    List.concat_map Region.all ~f:(fun region ->
      List.concat_map Service.all ~f:(fun service ->
        let credential_scope =
          Endpoint_rules_for_precompute.lookup_credential_scope ep ~region service
        in
        [ region, service, credential_scope ]))
  in
  let credential_scope_cases =
    List.map credential_scope_rules ~f:(fun (region, service, credential_scope) ->
      let region_s = Region.to_string region in
      let service_s = Service.to_string service in
      Ast_helper.Exp.case
        Ast_convenience.(ptuple [ pstr region_s; pstr service_s ])
        (Ast_convenience.str credential_scope))
    @ [ gen_catch_all
          [%expr failwithf "unknown credential scope for %s, %s" region service ()]
      ]
  in
  let credential_scope_match_with =
    Ast_helper.Exp.match_ [%expr region, service] credential_scope_cases
  in
  [%stri
    let lookup_credential_scope ~region service =
      let region = Region.to_string region in
      let service = Service.to_string service in
      let credential_scope_s = [%e credential_scope_match_with] in
      Region.of_string credential_scope_s
    ;;]
;;
