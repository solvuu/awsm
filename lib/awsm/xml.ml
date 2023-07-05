open! Import

module My_ezxmlm = struct
  (* This code in this 'My_ezxmlm' module is from the ezxmlm project, which
     is ISC Licensed: https://github.com/mirage/ezxmlm/blob/master/LICENSE.md

    Copyright (c) <the authors, see individual headers on files>

    Permission to use, copy, modify, and distribute this software for any
    purpose with or without fee is hereby granted, provided that the above
    copyright notice and this permission notice appear in all copies.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

  (* As per the instructions at the ezxmlm repo, if you are a sophisticated
     user you should probably use Xmlm directly. We're doing so, and carrying
     over a few of the convenience functions from the library. *)
  let from_input i =
    try
      let el tag children = `El (tag, children) in
      let data d = `Data d in
      Xmlm.input_doc_tree ~el ~data i
    with
    | Xmlm.Error ((line, col), err) ->
      let err = Xmlm.error_message err in
      raise (Failure (Printf.sprintf "[line %d, col %d] %s" line col err))
  ;;

  let from_string buf =
    let i = Xmlm.make_input (`String (0, buf)) in
    let dtd, doc = from_input i in
    dtd, [ doc ]
  ;;
end

type t = ('a Xmlm.frag as 'a) Xmlm.frag

let all_children t =
  match t with
  | `Data _ -> []
  | `El (_, all_children) -> all_children
;;

let filter_nodes ~with_tag:tag xs =
  List.filter xs ~f:(function
    | `Data _ -> false
    | `El (((_, child_tag), _), _) -> String.equal child_tag tag)
;;

let children t tag = all_children t |> filter_nodes ~with_tag:tag

let child t tag =
  match t with
  | `Data _ -> None
  | `El (((_, node_tag), _), all_children) -> (
    match filter_nodes ~with_tag:tag all_children with
    | [] -> None
    | [ x ] -> Some x
    | _ ->
      failwithf "Xml.child: more than one child with tag %s in node %s" tag node_tag ())
;;

let child_exn ?context t tag_name =
  match children t tag_name with
  | [ arg ] -> arg
  | xs -> (
    let context = Option.value_map context ~default:"" ~f:(sprintf " in %s") in
    match xs with
    | [] -> failwithf "Missing %s child in %s node" tag_name context ()
    | _ -> failwithf "Expected only one %s child in %s node" tag_name context ())
;;

let string_data_exn ?context = function
  | `El (_, [ `Data d ]) -> d
  | `El (_, []) -> ""
  | _ ->
    let context = Option.value_map context ~default:"" ~f:(sprintf " in %s") in
    failwithf "Expected a node with one pcdata child%s" context ()
;;

let leaf root d = [ `El ((("", root), []), [ `Data d ]) ]
let node root children = [ `El ((("", root), []), List.concat children) ]

let rec of_value root = function
  | `Boolean b -> leaf root (Bool.to_string b)
  | `Long l -> leaf root (Int64.to_string l)
  | `Float f -> leaf root (Float.to_string f)
  | `Double d -> leaf root (Float.to_string d)
  | `Integer i -> leaf root (Int.to_string i)
  | `Blob b -> leaf root b
  | `Timestamp t -> leaf root t
  | `String s -> leaf root s
  | `Structure fields -> node root (List.map fields ~f:(fun (k, v) -> of_value k v))
  | `List xs -> List.map xs ~f:(of_value root) |> List.concat
  | `Enum e -> leaf root e
  | `Map _ -> assert false
;;

(* FIXME: uninmplemented *)

let of_values root fields : t =
  let children =
    List.map fields ~f:(fun (field_name, value) -> of_value field_name value)
  in
  `El ((("", root), []), List.concat children)
;;

let rec to_string = function
  | `El (((_, tag), _), children) ->
    sprintf "<%s>%s</%s>" tag (String.concat ~sep:"" (List.map children ~f:to_string)) tag
  | `Data d -> d
;;

let only_exn = function
  | [ x ] -> x
  | [] -> failwith "only_exn: returned no results"
  | _ :: _ :: _ -> failwith "only_exn: returner more than one result"
;;

let from_string = My_ezxmlm.from_string
let parse_response s = My_ezxmlm.from_string s |> snd |> only_exn
