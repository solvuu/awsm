open! Core
open! Import

let type_declaration ?kind ?manifest ?priv n =
  let loc = !Ast_helper.default_loc in
  Ast_helper.Type.mk
    (Ast_convenience.mknoloc (Shape.uncapitalized_id n))
    ?manifest
    ?kind
    ?priv
    ~attrs:
      [ { attr_name = Ast_convenience.mknoloc "deriving"
        ; attr_payload = PStr [%str sexp]
        ; attr_loc = loc
        }
      ]
;;

let error_cases (op : Botodata.operation) =
  let loc = !Ast_helper.default_loc in
  let case name typ =
    { prf_desc = Rtag ({ txt = name; loc = Location.none }, false, [ typ ])
    ; prf_loc = Location.none
    ; prf_attributes = []
    }
  in
  let error_cases =
    op.errors
    |> Option.value ~default:[]
    |> List.map ~f:(fun { shape; _ } -> shape)
    |> List.dedup_and_sort ~compare:String.compare
    |> List.map ~f:(fun shape ->
         case (Shape.capitalized_id shape) (Shape.core_type_of_shape shape))
  in
  (*let name = sprintf "%s_error" (Shape.uncapitalized_id op.name) in*)
  let name = "error" in
  let catch_all_error_case =
    case "Unknown_operation_error" [%type: string * string option]
  in
  ( name
  , Ast_helper.Typ.mk
      (Ptyp_variant (error_cases @ [ catch_all_error_case ], Closed, None)) )
;;

let type_declaration_of_errors op =
  let name, manifest = error_cases op in
  type_declaration ~manifest name
;;

let%expect_test "type_declaration_of_errors" =
  let test op =
    let tdecl = type_declaration_of_errors op in
    printf
      "%s%!\n"
      (Util.structure_to_string [ Ast_helper.Str.type_ Nonrecursive [ tdecl ] ])
  in
  let error shape =
    { Botodata.shape
    ; documentation = None
    ; exception_ = None
    ; fault = None
    ; error = None
    ; xmlOrder = None
    }
  in
  let operation errors =
    { Botodata.name = "name"
    ; http = { method_ = `GET; requestUri = []; responseCode = None }
    ; input = None
    ; output = None
    ; errors
    ; documentation = None
    ; documentationUrl = None
    ; alias = None
    ; deprecated = None
    ; deprecatedMessage = None
    ; authtype = None
    ; idempotent = None
    ; httpChecksum = None
    ; endpoint = None
    ; endpointdiscovery = None
    }
  in
  test (operation None);
  [%expect
    {|
    type nonrec error = [ `Unknown_operation_error of (string * string option) ]
    [@@deriving sexp] |}];
  test (operation (Some []));
  [%expect
    {|
    type nonrec error = [ `Unknown_operation_error of (string * string option) ]
    [@@deriving sexp] |}];
  test (operation (Some [ error "error_a"; error "error_b" ]));
  [%expect
    {|
    type nonrec error =
      [ `Error_a of Error_a.t  | `Error_b of Error_b.t
      | `Unknown_operation_error of (string * string option) ][@@deriving sexp] |}]
;;

let type_alias ?priv manifest = type_declaration ?priv "t" ~manifest

(** A field typed like its name, such as [t : t]. *)
let self_typed_field raw_name =
  let name = Shape.uncapitalized_id raw_name in
  Ast_helper.Type.field
    (Ast_convenience.mknoloc name)
    (Ast_helper.Typ.constr (Ast_convenience.lid name) [])
;;

let type_declarations_of_shape ?result_wrapper ?priv shape =
  let loc = !Ast_helper.default_loc in
  match shape with
  | Botodata.Boolean_shape _ -> [ type_alias ?priv [%type: bool] ]
  | Float_shape _ -> [ type_alias ?priv [%type: float] ]
  | Integer_shape _ -> [ type_alias ?priv [%type: int] ]
  | String_shape _ -> [ type_alias ?priv [%type: string] ]
  | Long_shape _ -> [ type_alias ?priv [%type: Int64.t] ]
  | Double_shape _ -> [ type_alias ?priv [%type: float] ]
  | Timestamp_shape _ -> [ type_alias ?priv [%type: string] ]
  | Blob_shape _ -> [ type_alias ?priv [%type: string] ]
  | Enum_shape es ->
    let cases =
      List.map es.cases ~f:(fun case ->
        Ast_helper.Type.constructor (Ast_convenience.mknoloc (Shape.capitalized_id case)))
    in
    let other_case = Enum_other.type_decl ~loc in
    [ type_declaration ?priv "t" ~kind:(Ptype_variant (cases @ [ other_case ])) ]
  | List_shape ls ->
    let elem = Shape.core_type_of_shape ls.member.shape in
    [ type_alias ?priv [%type: [%t elem] list] ]
  | Map_shape ms ->
    let key = Shape.core_type_of_shape ms.key in
    let value = Shape.core_type_of_shape ms.value in
    [ type_alias ?priv [%type: ([%t key] * [%t value]) list] ]
  | Structure_shape ss -> (
    let unwrapped_shape_declaration type_name =
      match ss.members with
      | [] -> type_declaration ?priv type_name ~manifest:[%type: unit]
      | members ->
        let fields =
          List.map members ~f:(fun (fn, sm) ->
            (*
              let fn =
                match sm.locationName with
                | None -> fn
                | Some location_name -> location_name
              in
              *)
            let ty = Shape.core_type_of_shape sm.shape in
            let ty =
              if Shape.structure_shape_required_field ss fn
              then ty
              else [%type: [%t ty] option]
            in
            Ast_helper.Type.field (Ast_convenience.mknoloc (Shape.uncapitalized_id fn)) ty)
        in
        type_declaration ?priv type_name ~kind:(Ptype_record fields)
    in
    match result_wrapper with
    | None -> [ unwrapped_shape_declaration "t" ]
    | Some result_wrapper ->
      [ unwrapped_shape_declaration result_wrapper
      ; type_declaration
          ?priv
          (Shape.uncapitalized_id Shape.response_metadata_shape_name)
          ~manifest:[%type: unit]
      ; type_declaration
          ?priv
          "t"
          ~kind:
            (Ptype_record
               [ self_typed_field result_wrapper
               ; self_typed_field Shape.response_metadata_shape_name
               ])
      ])
;;

let%expect_test "type_declarations_of_shape" =
  let test ?result_wrapper shape =
    let tdecls = type_declarations_of_shape ?result_wrapper shape in
    printf
      "%s%!\n"
      (Util.structure_to_string [ Ast_helper.Str.type_ Nonrecursive tdecls ])
  in
  test (Boolean_shape { box = None; documentation = None });
  [%expect {| type nonrec t = bool[@@deriving sexp] |}];
  test (Float_shape { box = None; min = None; max = None; documentation = None });
  [%expect {| type nonrec t = float[@@deriving sexp] |}];
  test
    (Integer_shape
       { box = None
       ; min = None
       ; max = None
       ; documentation = None
       ; deprecated = None
       ; deprecatedMessage = None
       });
  [%expect {| type nonrec t = int[@@deriving sexp] |}];
  test
    (String_shape
       { pattern = None
       ; min = None
       ; max = None
       ; sensitive = None
       ; documentation = None
       ; deprecated = None
       ; deprecatedMessage = None
       });
  [%expect {| type nonrec t = string[@@deriving sexp] |}];
  test (Long_shape { box = None; min = None; max = None; documentation = None });
  [%expect {| type nonrec t = Int64.t[@@deriving sexp] |}];
  test (Double_shape { box = None; documentation = None; min = None; max = None });
  [%expect {| type nonrec t = float[@@deriving sexp] |}];
  test (Timestamp_shape { timestampFormat = None; documentation = None });
  [%expect {| type nonrec t = string[@@deriving sexp] |}];
  test
    (Blob_shape
       { streaming = None
       ; sensitive = None
       ; min = None
       ; max = None
       ; documentation = None
       });
  [%expect {| type nonrec t = string[@@deriving sexp] |}];
  test
    (Enum_shape
       { cases = [ "a"; "b"; "c" ]
       ; documentation = None
       ; min = None
       ; max = None
       ; pattern = None
       ; deprecatedMessage = None
       ; deprecated = None
       ; sensitive = None
       });
  [%expect
    {|
    type nonrec t =
      | A
      | B
      | C
      | Non_static_id of string [@@deriving sexp] |}];
  let member_shape shape =
    { Botodata.shape
    ; deprecated = None
    ; deprecatedMessage = None
    ; location = None
    ; locationName = None
    ; documentation = None
    ; xmlNamespace = None
    ; streaming = None
    ; xmlAttribute = None
    ; queryName = None
    ; box = None
    ; flattened = None
    ; idempotencyToken = None
    ; eventpayload = None
    ; hostLabel = None
    ; jsonvalue = None
    }
  in
  test
    (List_shape
       { min = None
       ; max = None
       ; documentation = None
       ; flattened = None
       ; member = member_shape "member_shape"
       ; sensitive = None
       ; deprecated = None
       ; deprecatedMessage = None
       });
  [%expect {| type nonrec t = Member_shape.t list[@@deriving sexp] |}];
  test
    (Map_shape
       { key = "key"
       ; value = "value"
       ; min = None
       ; max = None
       ; flattened = None
       ; locationName = None
       ; documentation = None
       ; sensitive = None
       });
  [%expect {| type nonrec t = (Key.t * Value.t) list[@@deriving sexp] |}];
  let structure_shape members =
    Botodata.Structure_shape { Botodata.empty_structure_shape with members }
  in
  test (structure_shape []);
  [%expect {| type nonrec t = unit[@@deriving sexp] |}];
  let nonempty_structure =
    structure_shape
      [ "name_a", member_shape "member_a"; "name_b", member_shape "member_b" ]
  in
  test nonempty_structure;
  [%expect
    {|
    type nonrec t = {
      name_a: Member_a.t option ;
      name_b: Member_b.t option }[@@deriving sexp] |}];
  test ~result_wrapper:"result_wrapper" nonempty_structure;
  [%expect
    {|
    type nonrec result_wrapper =
      {
      name_a: Member_a.t option ;
      name_b: Member_b.t option }[@@deriving sexp]
    and responseMetaData = unit[@@deriving sexp]
    and t = {
      result_wrapper: result_wrapper ;
      responseMetaData: responseMetaData }[@@deriving sexp] |}]
;;
