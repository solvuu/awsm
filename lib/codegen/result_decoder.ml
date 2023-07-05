open! Core
open! Import

type t =
  | Xml
  | Json
  | Of_header_and_body of string option
[@@deriving sexp_of]

let of_botodata ~default (op : Botodata.operation) ~shapes =
  Option.map op.output ~f:(fun { shape = output_shape_name; _ } ->
    let (output_shape : Botodata.shape) =
      List.Assoc.find_exn shapes ~equal:String.equal output_shape_name
    in
    match Shape.shape_is_header_structure' ~shapes output_shape with
    | false -> default
    | true ->
      let has_body =
        match output_shape with
        | Structure_shape { payload; _ } -> payload
        | _ -> None
      in
      Of_header_and_body has_body)
;;

let of_botodata_xml (op : Botodata.operation) ~shapes =
  of_botodata ~default:Xml op ~shapes
;;

let of_botodata_json (op : Botodata.operation) ~shapes =
  of_botodata ~default:Json op ~shapes
;;
