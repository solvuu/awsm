open! Core
open! Import

type t =
  { name : string
  ; field_name : string
  ; shape : string
  ; is_required : bool
  }
[@@deriving fields, sexp_of]

let create = Fields.create
let param_name = name

let convert p =
  Printf.ksprintf Ast_convenience.evar "%s.to_header" (Shape.capitalized_id p.shape)
;;

let of_input_shape (structure_shape : Botodata.structure_shape) =
  List.filter_map
    structure_shape.members
    ~f:(fun (key, { location; locationName; shape; _ }) ->
    match location with
    | Some `querystring ->
      let name = Option.value locationName ~default:shape in
      let is_required =
        structure_shape.Botodata.required
        |> Option.value ~default:[]
        |> fun l -> List.mem ~equal:String.equal l key
      in
      Some (create ~name ~shape ~is_required ~field_name:key)
    | _ -> None)
;;

let of_botodata (op : Botodata.operation) ~shapes =
  let find_shape shape = List.Assoc.find_exn shapes shape ~equal:String.equal in
  match op.input with
  | None -> []
  | Some { shape; _ } -> (
    match find_shape shape with
    | Botodata.Structure_shape structure_shape -> of_input_shape structure_shape
    | _ -> [])
;;
