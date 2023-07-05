module String = struct
  include String

  let matches x ~pat =
    match Re.exec Re.Perl.(compile_pat pat) x with
    | _ -> true
    | exception Caml.Not_found -> false
  ;;
end

module Test = struct
  let pass msg alist =
    printf "[OK] %s\n" msg;
    List.iter alist ~f:(fun (x, y) -> printf "%s: %s\n" x y)
  ;;

  let fail msg alist =
    printf "[FIXME] %s\n" msg;
    List.iter alist ~f:(fun (x, y) -> printf "%s: %s\n" x y)
  ;;
end

let ensure b fmt = Format.kasprintf (fun s -> if b then Ok () else Error s) fmt

let check_string_min s ~min =
  ensure (Caml.String.length s >= min) "Expected string of length greater than %d" min
;;

let%expect_test "check_string_min" =
  let test s =
    check_string_min s ~min:3 |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test "ab";
  [%expect {| (Error "Expected string of length greater than 3") |}];
  test "abc";
  [%expect {| (Ok ()) |}];
  test "abcd";
  [%expect {| (Ok ()) |}]
;;

let check_string_max s ~max =
  ensure (Caml.String.length s <= max) "Expected string of length less than %d" max
;;

let%expect_test "check_string_max" =
  let test s =
    check_string_max s ~max:3 |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test "ab";
  [%expect {| (Ok ()) |}];
  test "abc";
  [%expect {| (Ok ()) |}];
  test "abcd";
  [%expect {| (Error "Expected string of length less than 3") |}]
;;

let unicode_class_pat = Re.Perl.compile_pat "(\\\\p\\{[MNSLP]\\})|(\\\\u[0-9]{4})"
let unicode_pat = Re.Perl.compile_pat "\\\\u([0-9A-F][0-9A-F])([0-9A-F][0-9A-F])"

let map_unicode_pattern_to_ascii_pattern s =
  let s =
    Re.replace ~all:true unicode_class_pat s ~f:(fun group ->
      let x = Re.Group.get group 0 in
      match x with
      | "\\p{L}" -> "a-zA-Z"
      | "\\p{P}" -> "!\"\\#$%&'()*+,\\-./:;<=>?@\\[\\\\\\]^_`{|}~"
      | "\\p{S}" -> ""
      | "\\p{N}" -> "0-9"
      | "\\p{M}" -> ""
      | "\\p{Z}" -> "\\t\\r\\n\\v\\f"
      | _ -> x
      (* pass through *))
  in
  Re.replace ~all:true unicode_pat s ~f:(fun group ->
    let major, minor =
      Array.foldi (Re.Group.all group) ~init:([], []) ~f:(fun i (major, minor) s ->
        match i with
        | 0 -> major, minor
        | 1 | 3 -> major @ [ "\\x" ^ s ], minor
        | 2 | 4 -> major, minor @ [ "\\x" ^ s ]
        | x -> failwithf "unexpected number of groups %d" x ())
    in
    let to_range range = String.concat range ~sep:"-" in
    let _major_string = to_range major in
    let _minor_string = to_range minor in
    "a-zA-Z0-9_-"
    (* major_string ^ minor_string *))
;;

let check_pattern s ~pattern =
  let pat = map_unicode_pattern_to_ascii_pattern pattern in
  ensure
    (Re.execp (Re.Perl.compile_pat pat) s)
    "String %s doesn't match expected regex %s"
    s
    pat
;;

let%expect_test "check_pattern" =
  let test s =
    check_pattern s ~pattern:{|\d|} |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test "abc";
  [%expect {| (Error "String abc doesn't match expected regex \\d") |}];
  test "a0c";
  [%expect {| (Ok ()) |}]
;;

let check_list_min l ~min =
  ensure (List.length l >= min) "Expected list of length greater than %d" min
;;

let%expect_test "check_list_min" =
  let test l =
    check_list_min l ~min:3 |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test [ 1; 2 ];
  [%expect {| (Error "Expected list of length greater than 3") |}];
  test [ 1; 2; 3 ];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3; 4 ];
  [%expect {| (Ok ()) |}]
;;

let check_list_max l ~max =
  ensure (List.length l <= max) "Expected list of length less than %d" max
;;

let%expect_test "check_list_max" =
  let test l =
    check_list_max l ~max:3 |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test [ 1; 2 ];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3 ];
  [%expect {| (Ok ()) |}];
  test [ 1; 2; 3; 4 ];
  [%expect {| (Error "Expected list of length less than 3") |}]
;;

let check_int_min m ~min = ensure (m >= min) "Expected integer greater than %d" min

let%expect_test "check_int_min" =
  let test n = check_int_min n ~min:3 |> [%sexp_of: (unit, string) Result.t] |> print_s in
  test 2;
  [%expect {| (Error "Expected integer greater than 3") |}];
  test 3;
  [%expect {| (Ok ()) |}];
  test 4;
  [%expect {| (Ok ()) |}]
;;

let check_int_max m ~max = ensure (m <= max) "Expected less than %d" max

let%expect_test "check_int_max" =
  let test n = check_int_max n ~max:3 |> [%sexp_of: (unit, string) Result.t] |> print_s in
  test 2;
  [%expect {| (Ok ()) |}];
  test 3;
  [%expect {| (Ok ()) |}];
  test 4;
  [%expect {| (Error "Expected less than 3") |}]
;;

let check_int64_min i ~min =
  ensure (Int64.( >= ) i min) "Expected long greater than %Ld" min
;;

let check_float_min m ~min:fmin =
  ensure Float.(m >= fmin) "Expected float greater than or equal to %f" fmin
;;

let%expect_test "check_float_min" =
  let test n =
    check_float_min n ~min:3. |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test 2.;
  [%expect {| (Error "Expected float greater than or equal to 3.000000") |}];
  test 3.;
  [%expect {| (Ok ()) |}];
  test 4.;
  [%expect {| (Ok ()) |}]
;;

let check_float_max m ~max:fmax = ensure Float.(m <= fmax) "Expected less than %f" fmax

let%expect_test "check_float_max" =
  let test n =
    check_float_max n ~max:3. |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test 2.;
  [%expect {| (Ok ()) |}];
  test 3.;
  [%expect {| (Ok ()) |}];
  test 4.;
  [%expect {| (Error "Expected less than 3.000000") |}]
;;

let%expect_test "check_int64_min" =
  let test n =
    check_int64_min n ~min:3L |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test 2L;
  [%expect {| (Error "Expected long greater than 3") |}];
  test 3L;
  [%expect {| (Ok ()) |}];
  test 4L;
  [%expect {| (Ok ()) |}]
;;

let check_int64_max i ~max = ensure (Int64.( <= ) i max) "Expected long less than %Ld" max

let%expect_test "check_int64_max" =
  let test n =
    check_int64_max n ~max:3L |> [%sexp_of: (unit, string) Result.t] |> print_s
  in
  test 2L;
  [%expect {| (Ok ()) |}];
  test 3L;
  [%expect {| (Ok ()) |}];
  test 4L;
  [%expect {| (Error "Expected long less than 3") |}]
;;

let string_of_xml ~kind = function
  | `El (_, [ `Data d ]) -> d
  | _ -> failwithf "Expected pcdata representing %s" kind ()
;;

let bool_of_json = function
  | `Bool b -> b
  | _ -> failwith "Expected a boolean"
;;

let float_of_json ~kind = function
  | `Float f -> f
  | `Int i -> Float.of_int i
  | _ -> failwithf "Expected float representing %s" kind ()
;;

let string_of_json ~kind = function
  | `String s -> s
  | _ -> failwithf "Expected string representing %s" kind ()
;;

let timestamp_of_json = function
  | `String s -> s
  | `Float f -> Float.to_string f
  | _ -> failwith "Expected string representing a timestamp"
;;

let list_of_json ~kind ~of_json = function
  | `List xs -> List.map xs ~f:of_json
  | _ -> failwithf "Expected json list to parse %s" kind ()
;;

let object_of_json ~key_of_string ~of_json = function
  | `Assoc alst -> List.map alst ~f:(fun (key, json) -> key_of_string key, of_json json)
  | _ -> failwith "Expected json representing a map"
;;
