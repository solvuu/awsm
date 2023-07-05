open! Import

module Error = struct
  type make =
    [ `Invalid_qualifier of string
    | `Invalid_account_id of string
    ]
  [@@deriving sexp]
end

module Default = struct
  let partition = "aws"
end

type resource_type =
  [ `Slash_delimited of string
  | `Colon_delimited of string
  | `None
  ]
[@@deriving sexp, compare]

type qualifier =
  [ `Slash_delimited of string
  | `Colon_delimited of string
  | `None
  ]
[@@deriving sexp, compare]

type t =
  { partition : string
  ; service : Service.t
  ; region : Region.t option
  ; account_id : string option
  ; resource : string
  ; resource_type : resource_type
  ; qualifier : qualifier
  }
[@@deriving sexp, compare]

let make
  ?(partition = Default.partition)
  ?region
  ?account_id
  ?(resource_type = `None)
  ?(qualifier = `None)
  ~service
  ~resource
  ()
  =
  let make_qualifier x =
    match x with
    | `None -> `Ok `None
    | `Slash_delimited s | `Colon_delimited s ->
      if String.equal s "" then `Invalid_qualifier s else `Ok x
  in
  let make_account_id x =
    Option.value_map x ~default:(`Ok None) ~f:(fun x ->
      if String.equal x "" then `Invalid_account_id x else `Ok (Some x))
  in
  match make_qualifier qualifier with
  | `Invalid_qualifier _ as x -> x
  | `Ok qualifier -> (
    match make_account_id account_id with
    | `Invalid_account_id _ as x -> x
    | `Ok account_id ->
      `Ok { partition; service; region; account_id; resource; resource_type; qualifier })
;;

let make_exn
  ?partition
  ?region
  ?account_id
  ?resource_type
  ?qualifier
  ~service
  ~resource
  ()
  =
  match
    make ?partition ?region ?account_id ?resource_type ?qualifier ~service ~resource ()
  with
  | `Ok x -> x
  | #Error.make as e -> failwithf !"Invalid ARN %{sexp:Error.make}" e ()
;;

let of_string s =
  let build partition service region account_id resource_type resource qualifier =
    let service = Service.of_string service in
    let region =
      match region with
      | "" -> None
      | _ -> Some (Region.of_string region)
    in
    let account_id =
      match account_id with
      | "" -> None
      | _ -> Some account_id
    in
    make_exn
      ~partition
      ~service
      ?region
      ?account_id
      ~resource
      ~resource_type
      ~qualifier
      ()
  in
  let split = String.split ~on:':' s in
  match split with
  (* arn:partition:service:region:account-id:resourcetype:resource:qualifier *)
  | [ "arn"; partition; service; region; account_id; resource_type; resource; qualifier ]
    ->
    build
      partition
      service
      region
      account_id
      (`Colon_delimited resource_type)
      resource
      (`Colon_delimited qualifier)
  | [ "arn"; partition; service; region; account_id; resource_part; last_part ] ->
    if String.contains resource_part '/'
    then (
      (* arn:partition:service:region:account-id:resourcetype/resource:qualifier *)
      let qualifier = last_part in
      let resource_type, resource = String.lsplit2_exn ~on:'/' resource_part in
      build
        partition
        service
        region
        account_id
        (`Slash_delimited resource_type)
        resource
        (`Colon_delimited qualifier))
    else (
      (* arn:partition:service:region:account-id:resourcetype:resource *)
      let resource = last_part in
      build
        partition
        service
        region
        account_id
        (`Colon_delimited resource_part)
        resource
        `None)
  (* arn:partition:service:region:account-id:resource
     arn:partition:service:region:account-id:resourcetype/resource
     arn:partition:service:region:account-id:resourcetype/resource/qualifier *)
  | [ "arn"
    ; partition
    ; service
    ; region
    ; account_id
    ; resource_type_slash_resource_slash_qualifier
    ] -> (
    match String.split ~on:'/' resource_type_slash_resource_slash_qualifier with
    | [ resource_type; resource; qualifier ] ->
      build
        partition
        service
        region
        account_id
        (`Slash_delimited resource_type)
        resource
        (`Slash_delimited qualifier)
    | [ resource_type; resource ] ->
      build
        partition
        service
        region
        account_id
        (`Slash_delimited resource_type)
        resource
        `None
    | [ resource ] -> build partition service region account_id `None resource `None
    | _ -> failwithf "'%s' has an invalid number of '/' delimiters." s ())
  | "arn" :: _ -> failwithf "'%s' has an invalid number of ':' delimiters." s ()
  | _ -> failwithf "'%s' is not an amazon resource name." s ()
;;

let%expect_test "of_string" =
  (* Strings expected to be valid ARNs. *)
  let good =
    [ (* Format arn:partition:service:region:account-id:resource *)
      [ "arn:partition:sqs:us-east-2:account-id:resource" ]
    ; (* Format arn:partition:service:region:account-id:resourcetype/resource *)
      [ "arn:partition:sqs:us-east-2:account-id:resourcetype/resource" ]
    ; (* Format
         arn:partition:service:region:account-id:resourcetype/resource/qualifier *)
      [ "arn:partition:sqs:us-east-2:account-id:resourcetype/resource/qualifier" ]
    ; (* Format
         arn:partition:service:region:account-id:resourcetype/resource:qualifier *)
      [ "arn:partition:sqs:us-east-2:account-id:resourcetype/resource:qualifier" ]
    ; (* Format arn:partition:service:region:account-id:resourcetype:resource *)
      [ (* Full *)
        "arn:partition:sqs:us-east-2:account-id:resourcetype:resource"
      ; (* Empty region *)
        "arn:partition:sqs::account-id:resourcetype:resource"
      ; (* Empty account *)
        "arn:partition:sqs:us-east-2::resourcetype:resource"
      ]
    ; (* Format
         arn:partition:service:region:account-id:resourcetype:resource:qualifier *)
      [ "arn:partition:sqs:us-east-2:account-id:resourcetype:resource:qualifier" ]
    ]
  in
  (* Strings expected to be invalid ARNs. *)
  let bad =
    [ (* Empty string. *)
      ""
    ; (* Invalid format. *)
      "x"
    ; (* Colon_delimited resource type with Slash_delimited qualifier. *)
      "arn:partition:sqs:us-east-2:account-id:resourcetype:resource/qualifier"
    ; (* Colon_delimited qualifier without resource type. *)
      "arn:partition:sqs:us-east-2:account-id::resource:qualifier"
    ; (* Slash_delimited qualifier without resource type. *)
      "arn:partition:sqs:us-east-2:account-id::resource/qualifier"
    ]
  in
  let test_bad input =
    match of_string input with
    | x ->
      Test.fail
        "Input unexpected to parse as ARN but did."
        [ "input", input; "arn", sprintf !"%{sexp:t}" x ]
    | exception e ->
      Test.pass
        "Correctly failed to parse input as an ARN."
        [ "input", input; "exception", Exn.to_string e ]
  in
  let test_good input =
    match of_string input with
    | x ->
      Test.pass "Input parsed as ARN." [ "input", input; "arn", sprintf !"%{sexp:t}" x ]
    | exception e ->
      Test.fail
        "Input failed to parse as ARN."
        [ "input", input; "exception", Exn.to_string e ]
  in
  List.iter (List.concat good) ~f:(fun x ->
    test_good x;
    printf "\n");
  List.iter bad ~f:(fun x ->
    test_bad x;
    printf "\n");
  [%expect
    {|
    [OK] Input parsed as ARN.
    input: arn:partition:sqs:us-east-2:account-id:resource
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource) (resource_type None)
     (qualifier None))

    [OK] Input parsed as ARN.
    input: arn:partition:sqs:us-east-2:account-id:resourcetype/resource
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource)
     (resource_type (Slash_delimited resourcetype)) (qualifier None))

    [OK] Input parsed as ARN.
    input: arn:partition:sqs:us-east-2:account-id:resourcetype/resource/qualifier
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource)
     (resource_type (Slash_delimited resourcetype))
     (qualifier (Slash_delimited qualifier)))

    [OK] Input parsed as ARN.
    input: arn:partition:sqs:us-east-2:account-id:resourcetype/resource:qualifier
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource)
     (resource_type (Slash_delimited resourcetype))
     (qualifier (Colon_delimited qualifier)))

    [OK] Input parsed as ARN.
    input: arn:partition:sqs:us-east-2:account-id:resourcetype:resource
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource)
     (resource_type (Colon_delimited resourcetype)) (qualifier None))

    [OK] Input parsed as ARN.
    input: arn:partition:sqs::account-id:resourcetype:resource
    arn: ((partition partition) (service sqs) (region ()) (account_id (account-id))
     (resource resource) (resource_type (Colon_delimited resourcetype))
     (qualifier None))

    [OK] Input parsed as ARN.
    input: arn:partition:sqs:us-east-2::resourcetype:resource
    arn: ((partition partition) (service sqs) (region (us-east-2)) (account_id ())
     (resource resource) (resource_type (Colon_delimited resourcetype))
     (qualifier None))

    [OK] Input parsed as ARN.
    input: arn:partition:sqs:us-east-2:account-id:resourcetype:resource:qualifier
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource)
     (resource_type (Colon_delimited resourcetype))
     (qualifier (Colon_delimited qualifier)))

    [OK] Correctly failed to parse input as an ARN.
    input:
    exception: (Failure "'' is not an amazon resource name.")

    [OK] Correctly failed to parse input as an ARN.
    input: x
    exception: (Failure "'x' is not an amazon resource name.")

    [FIXME] Input unexpected to parse as ARN but did.
    input: arn:partition:sqs:us-east-2:account-id:resourcetype:resource/qualifier
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource/qualifier)
     (resource_type (Colon_delimited resourcetype)) (qualifier None))

    [FIXME] Input unexpected to parse as ARN but did.
    input: arn:partition:sqs:us-east-2:account-id::resource:qualifier
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource)
     (resource_type (Colon_delimited ""))
     (qualifier (Colon_delimited qualifier)))

    [FIXME] Input unexpected to parse as ARN but did.
    input: arn:partition:sqs:us-east-2:account-id::resource/qualifier
    arn: ((partition partition) (service sqs) (region (us-east-2))
     (account_id (account-id)) (resource resource/qualifier)
     (resource_type (Colon_delimited "")) (qualifier None)) |}]
;;

let to_string
  { partition; service; region; account_id; resource; resource_type; qualifier }
  =
  let account_id = Option.value ~default:"" account_id in
  let region = Option.value_map ~default:"" ~f:Region.to_string region in
  match resource_type, qualifier with
  (* arn:partition:service:region:account-id:resource *)
  | `None, `None ->
    sprintf
      "arn:%s:%s:%s:%s:%s"
      partition
      (Service.to_string service)
      region
      account_id
      resource
  (* arn:partition:service:region:account-id:resourcetype/resource *)
  | `Slash_delimited resource_type, `None ->
    sprintf
      "arn:%s:%s:%s:%s:%s/%s"
      partition
      (Service.to_string service)
      region
      account_id
      resource_type
      resource
  (* arn:partition:service:region:account-id:resourcetype/resource/qualifier *)
  | `Slash_delimited resource_type, `Slash_delimited qualifier ->
    sprintf
      "arn:%s:%s:%s:%s:%s/%s/%s"
      partition
      (Service.to_string service)
      region
      account_id
      resource_type
      resource
      qualifier
  (* arn:partition:service:region:account-id:resourcetype/resource:qualifier *)
  | `Slash_delimited resource_type, `Colon_delimited qualifier ->
    sprintf
      "arn:%s:%s:%s:%s:%s/%s:%s"
      partition
      (Service.to_string service)
      region
      account_id
      resource_type
      resource
      qualifier
  (* arn:partition:service:region:account-id:resourcetype:resource *)
  | `Colon_delimited resource_type, `None ->
    sprintf
      "arn:%s:%s:%s:%s:%s:%s"
      partition
      (Service.to_string service)
      region
      account_id
      resource_type
      resource
  (* arn:partition:service:region:account-id:resourcetype:resource:qualifier *)
  | `Colon_delimited resource_type, `Colon_delimited qualifier ->
    sprintf
      "arn:%s:%s:%s:%s:%s:%s:%s"
      partition
      (Service.to_string service)
      region
      account_id
      resource_type
      resource
      qualifier
  | `Colon_delimited _, `Slash_delimited _
  | `None, `Colon_delimited _
  | `None, `Slash_delimited _ -> failwith "cannot build such arn"
;;

let s3 ?(partition = Default.partition) ~bucket ~key () =
  (* FIXME: Doing minimal validation at present. See #98 for information on what
     we really need to do. *)
  if String.equal bucket ""
  then `Invalid_bucket bucket
  else if String.equal key ""
  then `Invalid_key key
  else
    `Ok
      { partition
      ; service = Service.s3
      ; region = None
      ; account_id = None
      ; resource = key
      ; resource_type = `Slash_delimited bucket
      ; qualifier = `None
      }
;;

let s3_exn ?partition ~bucket ~key () : t =
  let sexp_of_error =
    [%sexp_of: [ `Invalid_bucket of string | `Invalid_key of string ]]
  in
  match s3 ?partition ~bucket ~key () with
  | `Ok x -> x
  | (`Invalid_bucket _ | `Invalid_key _) as x ->
    failwith (sexp_of_error x |> Sexp.to_string)
;;

module Exn = struct
  let make = make_exn
  let s3 = s3_exn
end
