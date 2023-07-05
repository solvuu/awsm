open! Awsm.Import
module Stdlib = Caml

module Attribute = struct
  type raw_attribute =
    { name : string
    ; value : string option
    }
  [@@deriving sexp]

  type t =
    [ `Unknown of raw_attribute
    | `Custom of raw_attribute
    | `Gender of string
    | `Family_name of string
    | `Locale of string
    | `Middle_name of string option
    | `Nickname of string
    | `Profile of string option
    | `Website of string option
    | `Picture of string
    | `Email of string
    | `Name of string
    | `Updated_at of string
    | `Preferred_user_name of string option
    | `Given_name of string
    ]
  [@@deriving sexp]
end

type attribute = Attribute.t [@@deriving sexp]

type t =
  { username : string
  ; attributes : attribute list
  ; access_token : string
  }
[@@deriving sexp]

type msg = string

let required_attribute t ~name ~f =
  match List.find_map t.attributes ~f with
  | Some x -> Ok x
  | None -> Error (sprintf "%s attribute required but not present" name)
;;

let required_attribute_exn t ~name ~f =
  Result.ok_or_failwith (required_attribute t ~name ~f)
;;

let%expect_test "required_attribute" =
  let call ~attributes =
    let user = { username = ""; attributes; access_token = "" } in
    let name = "name" in
    let f = function
      | `Name n -> Some n
      | _ -> None
    in
    required_attribute user ~name ~f
  in
  let test ~attributes =
    printf !"%{sexp: (string, string) Base.Result.t}" (call ~attributes)
  in
  (* Test 1: attribute 'name' found, value of 'name' attribute returned *)
  test ~attributes:[ `Name "foo" ];
  [%expect {|(Ok foo)|}];
  (* Test 2: attribute 'name' not found, error raised *)
  test ~attributes:[ `Email "foo" ];
  [%expect {|(Error "name attribute required but not present")|}]
;;

let optional_attribute t ~f = List.find_map t.attributes ~f

let%expect_test "optional_attribute" =
  let test ~attributes =
    let user = { username = ""; attributes; access_token = "" } in
    let f = function
      | `Name n -> Some n
      | _ -> None
    in
    let res = optional_attribute user ~f in
    printf !"%{sexp:string Option.t}" res
  in
  (* Test 1: attribute 'name' found, value of 'name' attribute returned *)
  test ~attributes:[ `Name "foo" ];
  [%expect {|(foo)|}];
  (* Test 2: attribute 'name' not found, no value returned *)
  test ~attributes:[ `Email "foo" ];
  [%expect {|()|}]
;;

let email t =
  required_attribute t ~name:"email" ~f:(function
    | `Email e -> Some e
    | _ -> None)
;;

let email_exn t = Result.ok_or_failwith (email t)

let%expect_test "email" =
  let call ~attributes = email { username = ""; attributes; access_token = "" } in
  let test ~attributes =
    printf !"%{sexp: (string, string) Base.Result.t}" (call ~attributes)
  in
  (* Test 1: attribute 'email' found, value of 'email' attribute returned *)
  test ~attributes:[ `Email "foo" ];
  [%expect {|(Ok foo)|}];
  (* Test 2: attribute 'email' not found, error raised *)
  test ~attributes:[ `Name "foo" ];
  [%expect {|(Error "email attribute required but not present")|}]
;;

let preferred_name t =
  optional_attribute t ~f:(function
    | `Preferred_user_name e -> Some e
    | _ -> None)
;;

let%expect_test "preferred_name" =
  let test ~attributes =
    let res = preferred_name { username = ""; attributes; access_token = "" } in
    printf !"%{sexp:string Option.t Option.t}" res
  in
  (* Test 1: attribute 'preferred_user_name' found, value returned *)
  test ~attributes:[ `Preferred_user_name (Some "foo") ];
  [%expect {|((foo))|}];
  (* Test 2: attribute 'preferred_user_name' found with no value, value returned *)
  test ~attributes:[ `Preferred_user_name None ];
  [%expect {|(())|}];
  (* Test 3: attribute 'preferred_user_name' not found *)
  test ~attributes:[ `Email "foo" ];
  [%expect {|()|}]
;;

let family_name t =
  required_attribute t ~name:"family_name" ~f:(function
    | `Family_name e -> Some e
    | _ -> None)
;;

let family_name_exn t = Result.ok_or_failwith (family_name t)

let%expect_test "family_name" =
  let call ~attributes = family_name { username = ""; attributes; access_token = "" } in
  let test ~attributes =
    printf !"%{sexp: (string, string) Base.Result.t}" (call ~attributes)
  in
  (* Test 1: attribute 'family_name' found, value returned *)
  test ~attributes:[ `Family_name "foo" ];
  [%expect {|(Ok foo)|}];
  (* Test 2: attribute 'family_name' not found, error raised *)
  test ~attributes:[ `Name "foo" ];
  [%expect {|(Error "family_name attribute required but not present")|}]
;;

let name t =
  optional_attribute t ~f:(function
    | `Name e -> Some e
    | _ -> None)
;;

let%expect_test "name" =
  let test ~attributes =
    let res = name { username = ""; attributes; access_token = "" } in
    printf !"%{sexp:string Option.t}" res
  in
  (* Test 1: attribute 'name' found, value returned *)
  test ~attributes:[ `Name "foo" ];
  [%expect {|(foo)|}];
  (* Test 2: attribute 'name' not found, no value returned *)
  test ~attributes:[ `Email "foo" ];
  [%expect {|()|}]
;;

module Exn = struct
  let required_attribute = required_attribute_exn
  let email = email_exn
  let family_name = family_name_exn
end
