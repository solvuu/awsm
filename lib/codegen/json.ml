open! Core
open! Import

(* This is a copy/paste of Yojson.Safe.t with `Tuple and `Variant removed.
   We mant the safe `Intlit loading without the other OCaml-specific extensions. *)
type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Intlit of string
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list
  ]
[@@deriving sexp]

exception Json_error of string

let rec conv = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Intlit s -> `Intlit s
  | `Float f -> `Float f
  | `String s -> `String s
  | `Assoc al -> `Assoc (List.map al ~f:(fun (k, v) -> k, conv v))
  | `List l -> `List (List.map l ~f:conv)
  | `Tuple _ ->
    failwith "Json.from_string: Yojson.Safe OCaml-specific tuples are not supported"
  | `Variant (v, _) ->
    failwithf
      "Json.from_string: Yojson.Safe OCaml-specific variants tags are not supported: %s"
      v
      ()
;;

let from_string s =
  try s |> Yojson.Safe.from_string |> conv with
  | Yojson.Json_error m -> raise (Json_error m)
;;

let%test "from_string exn" =
  try
    let (_ : t) = from_string "invalid-;l[[}" in
    false
  with
  | Json_error _ -> true
;;

let to_string (v : t) = Yojson.Safe.to_string (v :> Yojson.Safe.t)

module Util = struct
  exception Type_error of (string * t)

  let conv_exn f =
    try f () with
    | Yojson.Safe.Util.Type_error (m, t) -> raise (Type_error (m, conv t))
  ;;

  let member_or_null s (v : t) =
    conv_exn (fun () -> Yojson.Safe.Util.member s (v :> Yojson.Safe.t) |> conv)
  ;;

  let%test "member1" =
    match member_or_null "x" (`Assoc [ "x", `Int 1 ]) with
    | `Int 1 -> true
    | _ -> false
  ;;

  let%test "member2" =
    match member_or_null "x" (`Assoc [ "y", `Int 1 ]) with
    | `Null -> true
    | _ -> false
  ;;

  let%test "member3" =
    match member_or_null "x" (`String "z") with
    | _ -> false
    | exception Type_error _ -> true
  ;;

  (* Can't we make this an Option.try_with of field_map_exn? *)
  let field_map x field_name f =
    match x with
    | `Assoc fields -> (
      match List.Assoc.find fields field_name ~equal:String.equal with
      | None | Some `Null -> None
      | Some value -> Some (f value))
    | _ -> raise (Type_error ("Expected Assoc", x))
  ;;

  let field_map_exn x field_name f =
    match x with
    | `Assoc fields -> (
      match List.Assoc.find fields field_name ~equal:String.equal with
      | Some value -> f value
      | None -> raise (Type_error (sprintf "Expected field '%s'" field_name, x)))
    | _ -> raise (Type_error ("Expected Assoc", x))
  ;;
end

(*
type ('a, 's) stream = unit -> ('a option, 's) Monad.app
type await = [ `Await ]
type err = [ `Error of Jsonm.error ]
type eoi = [ `End ]

type scalar =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  ]

type error =
  [ Jsonm.error
  | `Unexpected_end_of_input
  | `Unexpected_end_of_object
  | `Unexpected_name of string
  | `Unexpected_lexeme of Jsonm.lexeme
  ]

let pp_error ppf = function
  | #Jsonm.error as err -> Jsonm.pp_error ppf err
  | `Unexpected_end_of_input -> Format.pp_print_string ppf "Unexpected end of input"
  | `Unexpected_lexeme v -> Format.fprintf ppf "Unexpected lexeme: %a" Jsonm.pp_lexeme v
  | `Unexpected_end_of_object -> Format.pp_print_string ppf "Unexpected end of object"
  | `Unexpected_name n -> Format.fprintf ppf "Unexpected name: %s" n
;;

(*
let stream_to_json { Monad.bind; return } stream k =
  let ( >>= ) = bind in
  let decoder = Jsonm.decoder `Manual in
  let error (`Error (err : Jsonm.error)) = return (Error (err :> error)) in
  let end_of_input `End = return (Error `Unexpected_end_of_input) in
  let rec refill k `Await =
    stream ()
    >>= function
    | Some x ->
      Jsonm.Manual.src decoder (Caml.Bytes.unsafe_of_string x) 0 (String.length x);
      k ()
    | None ->
      Jsonm.Manual.src decoder Caml.Bytes.empty 0 0;
      k ()
  and arr acc k =
    match Jsonm.decode decoder with
    | #await as v -> refill (fun () -> arr acc k) v
    | #err as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> base (fun v -> arr (v :: acc) k) v
  and name n k =
    match Jsonm.decode decoder with
    | #await as v -> refill (fun () -> name n k) v
    | #err as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> base (fun v -> k (n, v)) v
  and obj acc k =
    match Jsonm.decode decoder with
    | #await as v -> refill (fun () -> obj acc k) v
    | #err as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v -> return (Error (`Unexpected_lexeme v))
  and base k = function
    | #scalar as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> return (Error `Unexpected_end_of_object)
    | `Name n -> return (Error (`Unexpected_name n))
  and go k =
    match Jsonm.decode decoder with
    | #await as v -> refill (fun () -> go k) v
    | #err as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme (#Jsonm.lexeme as lexeme) -> base k lexeme
  in
  go k
;;
*)*)
