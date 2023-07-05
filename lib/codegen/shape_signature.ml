open! Core
open! Import

let of_json_arg_ty _shape =
  let loc = !Ast_helper.default_loc in
  let base = [%type: Awsm.Json.t] in
  [%type: [%t base]]
;;

let to_json_return_ty _shape =
  let loc = !Ast_helper.default_loc in
  [%type: Awsm.Json.t]
;;

let xml_ty shape =
  let loc = !Ast_helper.default_loc in
  let xml_t = [%type: Xml.t] in
  match shape with
  | Botodata.List_shape ls ->
    if Option.value ls.flattened ~default:false then [%type: [%t xml_t] list] else xml_t
  | Map_shape _
  | Structure_shape _
  | Boolean_shape _
  | Enum_shape _
  | Long_shape _
  | Integer_shape _
  | Float_shape _
  | Double_shape _
  | String_shape _
  | Blob_shape _
  | Timestamp_shape _ -> xml_t
;;

let of_xml_return_ty shape =
  let loc = !Ast_helper.default_loc in
  match shape with
  | Botodata.List_shape ls -> [%type: [%t Shape.core_type_of_shape ls.member.shape] list]
  | Map_shape _
  | Structure_shape _
  | Boolean_shape _
  | Enum_shape _
  | Long_shape _
  | Integer_shape _
  | Float_shape _
  | Double_shape _
  | String_shape _
  | Blob_shape _
  | Timestamp_shape _ -> [%type: t]
;;
