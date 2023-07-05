open! Core
open! Import

module Graph = struct
  (* Shape graph for the dependency relation

     This is necessary to order module definitions (one for each shape). *)

  module G = Graph.Persistent.Digraph.Concrete (String)
  include G
  module Dfs = Graph.Traverse.Dfs (G)
  module Topological = Graph.Topological.Make (G)
  module C = Graph.Components.Make (G)

  module Components = struct
    let fold ~on_scc ~on_vertex ~init_scc ~init_vertex graph =
      let scc = C.scc_list graph in
      let on_scc acc vs =
        let with_sig = List.length vs > 1 in
        let on_vertex v acc = on_vertex acc ~with_sig v in
        let folded = List.fold_left vs ~init:init_vertex ~f:on_vertex in
        on_scc acc folded
      in
      List.fold_left scc ~init:init_scc ~f:on_scc
    ;;
  end

  let of_service (s : Botodata.service) =
    let add_deps acc (s, shape) =
      match shape with
      | Botodata.Boolean_shape _
      | Long_shape _
      | Double_shape _
      | Float_shape _
      | Integer_shape _
      | String_shape _
      | Enum_shape _
      | Timestamp_shape _
      | Blob_shape _ -> acc
      | List_shape ls -> add_edge acc s ls.member.shape
      | Map_shape ms -> add_edge (add_edge acc s ms.key) s ms.value
      | Structure_shape ss ->
        List.fold ss.members ~init:acc ~f:(fun acc (_, sm) -> add_edge acc s sm.shape)
    in
    let vertices =
      List.fold s.shapes ~init:empty ~f:(fun acc (s, _) -> add_vertex acc s)
    in
    (* Most shapes completely list their dependencies as member shapes. *)
    let shapes_graph = List.fold s.shapes ~init:vertices ~f:add_deps in
    (* The normal shapes graph doesn't include error hierarchy. We need to trace through
       the set of service operation errors for that. *)
    List.fold s.operations ~init:shapes_graph ~f:(fun acc operation ->
      match operation.output, operation.errors with
      | None, _ | _, None -> acc
      | Some output, Some errors ->
        List.fold errors ~init:acc ~f:(fun acc error ->
          add_edge acc output.shape error.shape))
  ;;
end

(* The field of a structure shape is assumed not to be required unless
   explicitly stated *)
let structure_shape_required_field (ss : Botodata.structure_shape) field_name =
  match ss.required with
  | Some required -> List.mem required field_name ~equal:String.equal
  | None -> false
;;

let char_censor = function
  | ':' | '.' | '-' | '/' | '*' | ' ' | '(' | ')' -> '_'
  | c -> c
;;

let ocaml_keywords =
  [ "and"
  ; "as"
  ; "assert"
  ; "begin"
  ; "class"
  ; "constraint"
  ; "do"
  ; "done"
  ; "downto"
  ; "else"
  ; "end"
  ; "exception"
  ; "external"
  ; "false"
  ; "for"
  ; "fun"
  ; "function"
  ; "functor"
  ; "if"
  ; "in"
  ; "include"
  ; "inherit"
  ; "initializer"
  ; "lazy"
  ; "let"
  ; "match"
  ; "method"
  ; "module"
  ; "mutable"
  ; "new"
  ; "object"
  ; "of"
  ; "open"
  ; "or"
  ; "private"
  ; "rec"
  ; "sig"
  ; "struct"
  ; "then"
  ; "to"
  ; "true"
  ; "try"
  ; "type"
  ; "val"
  ; "virtual"
  ; "when"
  ; "while"
  ; "with"
  ]
;;

let capitalized_id x =
  let x =
    (* Some shape names start with _. Lets convert to Zz_ so they
       become valid module names. *)
    if String.is_prefix x ~prefix:"_" then "Zz" ^ x else x
  in
  let x = x |> String.capitalize |> String.map ~f:char_censor in
  (* Prevent collisions with standard library names. *)
  match x with
  | "Core" | "Format" | "Option" | "Result" | "String" | "Uri" -> x ^ "_"
  | x -> x
;;

let core_type_of_shape (shape_name : string) =
  Ast_helper.Typ.constr (Ast_convenience.lid (capitalized_id shape_name ^ ".t")) []
;;

(* generate uncapitalized id from a shape name.

   - Characters not allowed by OCaml lexical conventions are replaced by '_' - *)
let uncapitalized_id = function
  | "String" -> "string"
  | "Boolean" -> "bool"
  | "Float" -> "float"
  | "Integer" -> "int"
  | x ->
    let x =
      (if String.contains x '.' then x else String.uncapitalize x)
      |> String.map ~f:char_censor
    in
    if List.mem ocaml_keywords x ~equal:String.equal then x ^ "_" else x
;;

(* Predicate for the particular case of structure which are encoded in headers
   and possibly the body instead of just the body *)
let shape_is_header_structure' ~shapes = function
  | Botodata.Structure_shape ss ->
    List.for_all ss.members ~f:(fun (_, m) ->
      (* TODO use resolved polymorphic shape type to pattern match off *)
      (match List.Assoc.find_exn shapes m.shape ~equal:String.equal with
       | Botodata.Blob_shape _ -> true
       | _ -> false)
      || (match m.location with
          | Some `header -> true
          | _ -> false)
      ||
      match m.location with
      | Some `headers -> true
      | _ -> false)
  | Blob_shape _ -> true
  | _ -> false
;;

let shape_is_header_structure (service : Botodata.service) (shape : Botodata.shape) =
  shape_is_header_structure' ~shapes:service.shapes shape
;;

let response_metadata_shape_name = "ResponseMetaData"
