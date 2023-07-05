open! Core
open! Import

module Json_path = struct
  type fragment =
    | Key of string
    | Index of int
  [@@deriving sexp_of]

  let pp_fragment fmt = function
    | Key s -> Format.fprintf fmt ".%s" s
    | Index n -> Format.fprintf fmt "[%d]" n
  ;;

  let pp_path fmt fragments = List.rev fragments |> List.iter ~f:(pp_fragment fmt)

  type path = fragment list [@@deriving sexp_of]

  type 'a with_path =
    { value : 'a
    ; path : path
    }

  type t = Json.t with_path

  type error =
    { message : string
    ; error_path : path
    }
  [@@deriving sexp_of]

  let error_at x message = { message; error_path = x.path }

  let error_to_string { message; error_path } =
    if List.is_empty error_path
    then message
    else Format.asprintf "(at %a) %s" pp_path error_path message
  ;;

  type 'a or_error = ('a, error) Result.t [@@deriving sexp_of]

  let strip_loc x = x.value
  let at_root value = { value; path = [] }

  let list jp ~f =
    match jp.value with
    | `List js ->
      Result.all (List.mapi ~f:(fun i value -> f { value; path = Index i :: jp.path }) js)
    | _ -> Error (error_at jp "not a list")
  ;;

  type record = (string * Json.t) list with_path

  let error_at_record = error_at
  let get_field_in key r = List.Assoc.find r.value ~equal:String.equal key

  let field_opt key f r =
    match get_field_in key r with
    | Some value -> Result.map ~f:Option.some (f { value; path = Key key :: r.path })
    | None -> Ok None
  ;;

  let with_object jp k =
    match jp.value with
    | `Assoc fields -> k { value = fields; path = jp.path }
    | _ -> Error (error_at jp "not an object")
  ;;

  let dict parse jp =
    let f (k, value) =
      Result.map (parse { value; path = Key k :: jp.path }) ~f:(fun y -> k, y)
    in
    with_object jp (fun r -> Result.all (List.map ~f r.value))
  ;;

  let field_set r = String.Set.of_list (List.map ~f:fst r.value)
end

type 'a or_string = ('a, string) Result.t [@@deriving sexp_of]
type 'a t = T of (Json_path.t -> 'a Json_path.or_error)

let run (T f) j = Result.map_error ~f:Json_path.error_to_string (f (Json_path.at_root j))
let run_exn t j = run t j |> Result.ok_or_failwith

let parse_with f =
  T (fun j -> Result.map_error ~f:(Json_path.error_at j) (f (Json_path.strip_loc j)))
;;

let ( >>| ) (T x) f = T (fun j -> Result.map ~f (x j))

let map_result (T x) ~f =
  T
    (fun j ->
      Result.bind (x j) ~f:(fun x -> Result.map_error (f x) ~f:(Json_path.error_at j)))
;;

let ast = T (fun j -> Ok (Json_path.strip_loc j))

let string =
  map_result ast ~f:(function
    | `String s -> Ok s
    | _ -> Error "not a string")
;;

let float =
  map_result ast ~f:(function
    | `Float d -> Ok d
    (* json sucks *)
    | `Int i -> Ok (Float.of_int i)
    | _ -> Error "not a double")
;;

let%expect_test "string" =
  let test j = printf !"%{sexp:string or_string}\n" (run string j) in
  test (`String "s");
  [%expect {| (Ok s) |}];
  test (`Bool true);
  [%expect {| (Error "not a string") |}]
;;

let int =
  map_result ast ~f:(function
    | `Int n -> Ok n
    | `Intlit s -> Error (sprintf "%s exceeds int precision" s)
    | _ -> Error "not an int")
;;

let%expect_test "int" =
  let test j = printf !"%{sexp:int or_string}\n" (run int j) in
  test (`Int 1);
  [%expect {| (Ok 1) |}];
  (* (* make a proper intlit test *)
  test (`Intlit Int.max_value;
  [%expect {| (Error "not an exact integer") |}];
  *)
  test (`String "s");
  [%expect {| (Error "not an int") |}]
;;

let%expect_test "int strictness" =
  let test j = printf !"%{sexp:int or_string}\n" (run int j) in
  test (Json.from_string {| 1 |});
  [%expect {| (Ok 1) |}];
  test (Json.from_string {| 1.5 |});
  [%expect {| (Error "not an int") |}]
;;

let int64 =
  map_result ast ~f:(function
    | `Int n -> Ok (Int.to_int64 n)
    | `Intlit s -> (
      try Ok (Int64.of_string s) with
      | Failure _ -> Error (sprintf "%s exceeds int64 precision" s))
    | _ -> Error "not an int64")
;;

let%expect_test "int64" =
  let test j = printf !"%{sexp:int64 or_string}\n" (run int64 j) in
  test (`Int 1);
  [%expect {| (Ok 1) |}];
  test (`Intlit "9223372036854775807");
  [%expect {| (Ok 9223372036854775807) |}];
  test (`String "s");
  [%expect {| (Error "not an int64") |}]
;;

let bool =
  map_result ast ~f:(function
    | `Bool b -> Ok b
    | _ -> Error "not a boolean")
;;

let%expect_test "bool" =
  let test j = printf !"%{sexp:bool or_string}\n" (run bool j) in
  test (`Bool true);
  [%expect {| (Ok true) |}];
  test (`Bool false);
  [%expect {| (Ok false) |}];
  test (`String "s");
  [%expect {| (Error "not a boolean") |}]
;;

let exactly value =
  map_result string ~f:(fun s ->
    if String.equal s value then Ok () else Error "got a different value than expected")
;;

let%expect_test "exactly" =
  let test j = printf !"%{sexp:unit or_string}\n" (run (exactly "expected") j) in
  test (`String "expected");
  [%expect {| (Ok ()) |}];
  test (`String "other");
  [%expect {| (Error "got a different value than expected") |}];
  test (`Bool true);
  [%expect {| (Error "not a string") |}]
;;

let list (T f) = T (Json_path.list ~f)

let%expect_test "list" =
  let test j = printf !"%{sexp:int list or_string}\n" (run (list int) j) in
  test (`List [ `Int 1; `Int 2; `Int 3 ]);
  [%expect {| (Ok (1 2 3)) |}];
  test (`List [ `Int 1; `String "s"; `Int 3 ]);
  [%expect {| (Error "(at [1]) not an int") |}];
  test (`Float 1.);
  [%expect {| (Error "not a list") |}]
;;

type 'a record = R of (Json_path.record -> 'a Json_path.or_error * string list)

let return x = R (fun _ -> Ok x, [])

let map_result_f (R parse) ~f =
  R
    (fun record ->
      let r, w = parse record in
      let r2 =
        Result.bind r ~f:(fun y ->
          Result.map_error (f y) ~f:(Json_path.error_at_record record))
      in
      r2, w)
;;

module Let_syntax = struct
  let map x ~f = map_result_f x ~f:(fun y -> Ok (f y))

  let both (R parse_a) (R parse_b) =
    R
      (fun record ->
        let ra, wa = parse_a record in
        let rb, wb = parse_b record in
        let t = Result.Let_syntax.Let_syntax.both ra rb in
        t, wa @ wb)
  ;;
end

let field_opt key (T parse_field) =
  R
    (fun record ->
      let r = Json_path.field_opt key parse_field record in
      r, [ key ])
;;

let record (R f) =
  T
    (fun j ->
      Json_path.with_object j (fun fields ->
        let r, parsed_fields_list = f fields in
        let parsed_fields = String.Set.of_list parsed_fields_list in
        let all_fields = Json_path.field_set fields in
        let not_parsed = String.Set.diff all_fields parsed_fields in
        if String.Set.is_empty not_parsed
        then r
        else
          Error
            (Printf.ksprintf
               (Json_path.error_at j)
               !"some fields were not parsed: %{sexp: (String.Set.t)}"
               not_parsed)))
;;

let%expect_test "record" =
  let test j =
    let r =
      record
        (let%map x = field_opt "x" int
         and y = field_opt "y" string in
         x, y)
    in
    printf !"%{sexp:(int option * string option) or_string}\n" (run r j)
  in
  test (`Assoc [ "x", `Int 1; "y", `String "s" ]);
  [%expect {| (Ok ((1) (s))) |}];
  test (`Assoc [ "x", `Int 1 ]);
  [%expect {| (Ok ((1) ())) |}];
  test (`Assoc []);
  [%expect {| (Ok (() ())) |}];
  test (`Assoc [ "x", `Int 1; "y", `Bool true ]);
  [%expect {| (Error "(at .y) not a string") |}];
  test (`Bool true);
  [%expect {| (Error "not an object") |}];
  test (`Assoc [ "x", `Float 1.; "y", `Bool true; "z1", `Bool true; "z2", `Bool true ]);
  [%expect {| (Error "some fields were not parsed: (z1 z2)") |}]
;;

let field key parse =
  let error = sprintf "field %S was not found in record" key in
  map_result_f (field_opt key parse) ~f:(Result.of_option ~error)
;;

let%expect_test "field" =
  let test j =
    let r =
      record
        (let%map x = field "x" int
         and y = field "y" string in
         x, y)
    in
    printf !"%{sexp:(int * string) or_string}\n" (run r j)
  in
  test (`Assoc [ "x", `Int 1; "y", `String "s" ]);
  [%expect {| (Ok (1 s)) |}];
  test (`Assoc [ "x", `Int 1 ]);
  [%expect {| (Error "field \"y\" was not found in record") |}]
;;

let field_or key parse ~default =
  let%map x = field_opt key parse in
  Option.value ~default x
;;

let field_ignored k =
  let%map _ = field_opt k (parse_with (fun _ -> Ok ())) in
  ()
;;

let%expect_test "field_ignored" =
  let test j =
    let r =
      record
        (let%map x = field "x" int
         and () = field_ignored "y" in
         x)
    in
    printf !"%{sexp:int or_string}\n" (run r j)
  in
  test (`Assoc [ "x", `Int 1 ]);
  [%expect {| (Ok 1) |}];
  test (`Assoc [ "x", `Int 1; "y", `String "s" ]);
  [%expect {| (Ok 1) |}];
  test (`Assoc [ "x", `Int 1; "y", `Int 1 ]);
  [%expect {| (Ok 1) |}]
;;

let record_or_list_of element =
  T
    (fun j ->
      match Json_path.strip_loc j with
      | `List _ ->
        let (T parse) = list (record element) in
        parse j
      | `Assoc _ ->
        let (T parse) = record element in
        Result.map ~f:List.return (parse j)
      | _ -> Error (Json_path.error_at j "not an array nor an object"))
;;

let%expect_test "record_or_list_of" =
  let test j =
    let r =
      record_or_list_of
        (let%map x = field "x" int
         and y = field "y" int in
         x, y)
    in
    printf !"%{sexp:(int * int) list or_string}\n" (run r j)
  in
  test (`Assoc [ "x", `Int 1; "y", `Int 2 ]);
  [%expect {| (Ok ((1 2))) |}];
  test
    (`List [ `Assoc [ "x", `Int 1; "y", `Int 2 ]; `Assoc [ "x", `Int 3; "y", `Int 4 ] ]);
  [%expect {| (Ok ((1 2) (3 4))) |}];
  test (`String "s");
  [%expect {| (Error "not an array nor an object") |}]
;;

let field_based key get_parser =
  T
    (fun j ->
      let error fmt = Printf.ksprintf (fun e -> Error (Json_path.error_at j e)) fmt in
      Json_path.with_object j (fun fields ->
        match Json_path.get_field_in key fields with
        | None -> error "field not found: %s" key
        | Some (`String value) -> (
          match get_parser value with
          | Some r ->
            let (T parse) =
              record
                (let%map r = r
                 and () = field key (exactly value) in
                 r)
            in
            parse j
          | None -> error "unknown selector field: %S" value)
        | Some _ -> error "type is not a string"))
;;

let%expect_test "field_based" =
  let test j =
    let a = return (0, 0) in
    let b =
      let%map x = field "x" int in
      x, 0
    in
    let c =
      let%map x = field "x" int
      and y = field "y" int in
      x, y
    in
    let r =
      field_based "type" (function
        | "a" -> Some a
        | "b" -> Some b
        | "c" -> Some c
        | _ -> None)
    in
    printf !"%{sexp:(int * int) or_string}\n" (run r j)
  in
  test (`Assoc [ "type", `String "a" ]);
  [%expect {| (Ok (0 0)) |}];
  test (`Assoc [ "type", `String "b"; "x", `Int 1 ]);
  [%expect {| (Ok (1 0)) |}];
  test (`Assoc [ "type", `String "c"; "x", `Int 1; "y", `Int 2 ]);
  [%expect {| (Ok (1 2)) |}];
  test (`Assoc []);
  [%expect {| (Error "field not found: type") |}];
  test (`List []);
  [%expect {| (Error "not an object") |}];
  test (`Assoc [ "type", `Bool true ]);
  [%expect {| (Error "type is not a string") |}];
  test (`Assoc [ "type", `String "x" ]);
  [%expect {| (Error "unknown selector field: \"x\"") |}]
;;

let if_field_present key ~then_:(R t) ~else_:(R e) =
  R
    (fun record ->
      if Option.is_some (Json_path.get_field_in key record) then t record else e record)
;;

let%expect_test "if_field_present" =
  let test j =
    let x =
      let%map x1 = field "x1" int
      and x2 = field "x2" int
      and () = field_ignored "f" in
      x1 + x2
    in
    let y =
      let%map y1 = field "y1" int
      and y2 = field "y2" int in
      y1 + y2
    in
    let r = record (if_field_present "f" ~then_:x ~else_:y) in
    printf !"%{sexp:int or_string}\n" (run r j)
  in
  test (`Assoc [ "f", `String "a"; "x1", `Int 1; "x2", `Int 2 ]);
  [%expect {| (Ok 3) |}];
  test (`Assoc [ "y1", `Int 10; "y2", `Int 20 ]);
  [%expect {| (Ok 30) |}]
;;

let dict (T parse) = T (Json_path.dict parse)

let%expect_test "dict" =
  let test j =
    let r = dict int in
    printf !"%{sexp:(string * int) list or_string}\n" (run r j)
  in
  test (`Assoc [ "x", `Int 1; "y", `Int 2; "z", `Int 3 ]);
  [%expect {| (Ok ((x 1) (y 2) (z 3))) |}];
  test (`List []);
  [%expect {| (Error "not an object") |}]
;;

let%expect_test "location tracking" =
  let test r j = printf !"%{sexp:unit or_string}\n" (run (r >>| ignore) j) in
  let error = `Bool true in
  test
    (record (field "x" (record (field "y" int))))
    (`Assoc [ "x", `Assoc [ "y", error ] ]);
  [%expect {| (Error "(at .x.y) not an int") |}];
  test (record (field "x" (list int))) (`Assoc [ "x", `List [ `Int 3; `Int 6; error ] ]);
  [%expect {| (Error "(at .x[2]) not an int") |}];
  test (dict int) (`Assoc [ "x", `Int 1; "y", error ]);
  [%expect {| (Error "(at .y) not an int") |}]
;;
