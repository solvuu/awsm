open! Core
open! Import

let to_string_converter_of_enum_shape (es : Botodata.enum_shape) =
  let case c =
    Ast_helper.Exp.case
      (Ast_convenience.pconstr (Shape.capitalized_id c) [])
      (Ast_convenience.str c)
  in
  let other_case = Enum_other.to_string in
  Ast_helper.Exp.function_ (List.map es.cases ~f:case @ [ other_case ])
;;

let make_error_of_json shapes =
  let loc = !Ast_helper.default_loc in
  let body =
    let case c =
      let of_json = sprintf "%s.of_json" (Shape.capitalized_id c) in
      Ast_helper.Exp.case
        (Ast_convenience.pstr c)
        (Ast_convenience.variant
           (Shape.capitalized_id c)
           (Some (Ast_convenience.constr of_json [ [%expr json] ])))
    in
    let error_e =
      [%expr `Unknown_operation_error (name, Some (Awsm.Json.to_string json))]
    in
    List.map shapes ~f:case
    @ [ Ast_helper.Exp.case (Ast_convenience.pvar "name") error_e ]
    |> Ast_helper.Exp.match_ [%expr name]
  in
  [%stri let error_of_json name json = [%e body]]
;;

let make_error_of_xml shapes =
  let loc = !Ast_helper.default_loc in
  let body =
    let case c =
      let of_xml = sprintf "%s.of_xml" (Shape.capitalized_id c) in
      Ast_helper.Exp.case
        (Ast_convenience.pstr c)
        (Ast_convenience.variant
           (Shape.capitalized_id c)
           (Some (Ast_convenience.constr of_xml [ [%expr xml] ])))
    in
    let error_e =
      [%expr `Unknown_operation_error (name, Some (Awsm.Xml.to_string xml))]
    in
    List.map shapes ~f:case
    @ [ Ast_helper.Exp.case (Ast_convenience.pvar "name") error_e ]
    |> Ast_helper.Exp.match_ [%expr name]
  in
  [%stri let error_of_xml name xml = [%e body]]
;;

let to_value_converter_of_shape ?result_wrapper shape =
  let loc = !Ast_helper.default_loc in
  match shape with
  | Botodata.Boolean_shape _ -> [%expr fun x -> `Boolean x]
  | Long_shape _ -> [%expr fun x -> `Long x]
  | Double_shape _ -> [%expr fun x -> `Double x]
  | Integer_shape _ -> [%expr fun x -> `Integer x]
  | Float_shape _ -> [%expr fun x -> `Float x]
  | Timestamp_shape _ -> [%expr fun x -> `Timestamp x]
  | String_shape _ -> [%expr fun x -> `String x]
  | Blob_shape _ -> [%expr fun x -> `Blob x]
  | List_shape ls ->
    let member_conv =
      Ast_convenience.evar (Shape.capitalized_id ls.member.shape ^ ".to_value")
    in
    [%expr fun xs -> xs |> List.map ~f:[%e member_conv] |> fun x -> `List x]
  | Enum_shape _ -> [%expr fun x -> `Enum (to_string x)]
  | Structure_shape { members = []; _ } -> [%expr fun _ -> `Structure []]
  | Structure_shape ss -> (
    let field (field_name, shape_member) =
      let to_value =
        Ast_convenience.evar
          (Shape.capitalized_id shape_member.Botodata.shape ^ ".to_value")
      in
      let field =
        Ast_helper.Exp.field
          (Ast_convenience.evar "x")
          (Ast_convenience.lid (Shape.uncapitalized_id field_name))
      in
      let name = Option.value shape_member.Botodata.locationName ~default:field_name in
      if Shape.structure_shape_required_field ss field_name
      then [%expr [%e Ast_convenience.str name], Some ([%e to_value] [%e field])]
      else [%expr [%e Ast_convenience.str name], Option.map [%e field] ~f:[%e to_value]]
    in
    let list_expr = Ast_convenience.list (List.map ss.members ~f:field) in
    match result_wrapper with
    | None -> [%expr fun x -> structure_to_value [%e list_expr]]
    | Some result_wrapper ->
      [%expr
        fun t ->
          let x =
            [%e Ast_convenience.evar ("t." ^ Shape.uncapitalized_id result_wrapper)]
          in
          structure_to_wrapped_value
            [%e list_expr]
            ~wrapper:[%e Ast_convenience.str result_wrapper]
            ~response:[%e Ast_convenience.str Shape.response_metadata_shape_name]])
  | Map_shape ms ->
    let key_conv = Ast_convenience.evar (Shape.capitalized_id ms.key ^ ".to_value") in
    let value_conv = Ast_convenience.evar (Shape.capitalized_id ms.value ^ ".to_value") in
    [%expr
      fun xs ->
        xs
        |> List.map ~f:(fun (x, y) ->
             [%e key_conv] x |> fun x -> [%e value_conv] y |> fun y -> x, y)
        |> fun x -> `Map x]
;;

let to_query _ =
  let loc = !Ast_helper.default_loc in
  [%expr fun v -> to_query to_value v]
;;

let to_header_converter_of_shape shape =
  let loc = !Ast_helper.default_loc in
  match shape with
  | Botodata.Boolean_shape _ -> Some [%expr fun x -> Bool.to_string x]
  | Long_shape _ -> Some [%expr fun x -> Int64.to_string x]
  | Integer_shape _ -> Some [%expr fun x -> Int.to_string x]
  | Float_shape _ | Double_shape _ -> Some [%expr fun x -> Stdlib.Float.to_string x]
  | Timestamp_shape _ -> Some [%expr fun x -> x]
  | String_shape _ -> Some [%expr fun x -> x]
  | Blob_shape _ -> Some [%expr fun x -> x]
  | List_shape _ls ->
    Some
      [%expr fun _ -> failwithf "to_header is not implemented for List_shape objects" ()]
  | Enum_shape _ -> Some [%expr fun x -> to_string x]
  | Structure_shape _ -> None (* FIXME: unimplemented *)
  | Map_shape _ -> None
;;

let of_string_converter_of_shape shape =
  let loc = !Ast_helper.default_loc in
  match shape with
  | Botodata.Boolean_shape _ -> [%expr Bool.of_string]
  | Long_shape _ -> [%expr Int64.of_string]
  | Double_shape _ | Float_shape _ -> [%expr Float.of_string]
  | Integer_shape _ -> [%expr Int.of_string]
  | Blob_shape _ -> [%expr fun x -> x]
  | Timestamp_shape _ -> [%expr fun x -> x]
  | String_shape _ -> [%expr fun x -> x]
  | List_shape _ -> failwith "No of_string function for list shapes"
  | Enum_shape es ->
    let case c =
      Ast_helper.Exp.case
        (Ast_convenience.pstr c)
        (Ast_convenience.constr (Shape.capitalized_id c) [])
    in
    let other_e = Enum_other.of_string ~loc in
    List.map es.cases ~f:case @ [ Ast_helper.Exp.case (Ast_convenience.pvar "x") other_e ]
    |> Ast_helper.Exp.function_
  | Structure_shape _ -> [%expr fun _ -> assert false] (* FIXME: unimplemented *)
  | Map_shape _ ->
    [%expr
      fun _ -> failwith "of_string_converter_of_shape: Map_shape case not implemented"]
;;

let of_header_converter_of_map_shape
  (service : Botodata.service)
  (ms : Botodata.map_shape)
  =
  let location_name_prefix = Option.value ms.locationName ~default:"x-amz-meta-" in
  let location_name_prefix_evar = Ast_convenience.str location_name_prefix in
  let loc = !Ast_helper.default_loc in
  let get_constr name =
    match List.Assoc.find_exn service.shapes name ~equal:String.equal with
    | Structure_shape _ | Map_shape _ | List_shape _ -> None
    | _ -> Some (Ast_convenience.evar (Shape.capitalized_id name ^ ".of_string"))
  in
  let body =
    match get_constr ms.key, get_constr ms.value with
    | Some keyfn, Some valuefn -> [%expr [%e keyfn] chopped, [%e valuefn] v]
    | None, _ | _, None ->
      let s =
        sprintf "no of_header for complex types %s %s" ms.key ms.value
        |> Ast_convenience.str
      in
      [%expr
        let (_ : string) = v in
        let (_ : string) = chopped in
        failwith [%e s]]
  in
  let arguments =
    [%expr
      List.filter_map xs ~f:(fun (k, v) ->
        Base.String.chop_prefix k ~prefix:[%e location_name_prefix_evar]
        |> Option.map ~f:(fun chopped -> [%e body]))]
  in
  [%expr fun xs -> make [%e arguments]]
;;

let of_header_converter_of_structure_shape
  (service : Botodata.service)
  (ss : Botodata.structure_shape)
  =
  let loc = !Ast_helper.default_loc in
  let arguments =
    List.map ss.members ~f:(fun (field_name, member) ->
      let header_name =
        Option.value ~default:(String.lowercase field_name) member.locationName
      in
      let arg_id = Shape.uncapitalized_id field_name in
      let argument_is_required = Shape.structure_shape_required_field ss field_name in
      let assoc_e fn_e =
        Ast_convenience.app
          fn_e
          [ Ast_convenience.evar "xs"; Ast_convenience.str header_name ]
      in
      match
        List.Assoc.find service.shapes member.shape ~equal:String.equal
        |> Option.value_exn
             ~message:(sprintf "%s not found in service.shapes" member.shape)
      with
      | Map_shape _ ->
        let constructor_e =
          Ast_convenience.evar (Shape.capitalized_id member.shape ^ ".of_header")
        in
        if argument_is_required
        then (
          let converted_e = [%expr [%e constructor_e] xs] in
          let label = Labelled arg_id in
          label, converted_e)
        else (
          let label = Optional arg_id in
          label, [%expr Some ([%e constructor_e] xs)])
      | Blob_shape _ ->
        if argument_is_required
        then (
          let label = Labelled arg_id in
          label, [%expr pipe])
        else (
          let label = Optional arg_id in
          label, [%expr Some pipe])
      | _ ->
        let constructor_e =
          Ast_convenience.evar (Shape.capitalized_id member.shape ^ ".of_string")
        in
        let arg_id = Shape.uncapitalized_id field_name in
        if argument_is_required
        then (
          let converted_e =
            Ast_convenience.app
              constructor_e
              [ assoc_e [%expr List.Assoc.find_exn ~equal:String.Caseless.equal] ]
          in
          let label = Labelled arg_id in
          label, converted_e)
        else (
          let converted_e =
            [%expr
              Option.map
                [%e assoc_e [%expr List.Assoc.find ~equal:String.Caseless.equal]]
                ~f:[%e constructor_e]]
          in
          let label = Optional arg_id in
          label, converted_e))
  in
  Ast_convenience.lam
    [%pat? xs, pipe]
    (Ast_helper.Exp.apply [%expr make] (arguments @ [ Nolabel, Ast_convenience.unit () ]))
    ~attrs:
      [ { attr_name = Ast_convenience.mknoloc "warning"
        ; attr_loc = loc
        ; attr_payload =
            PStr
              [ { pstr_loc = Location.none
                ; pstr_desc =
                    Pstr_eval
                      ( { pexp_desc = Pexp_constant (Pconst_string ("-27", loc, None))
                        ; pexp_loc = Location.none
                        ; pexp_attributes = []
                        ; pexp_loc_stack = []
                        }
                      , [] )
                }
              ]
        }
      ]
;;

let of_xml_converter_of_shape ?result_wrapper context_is_referenced service shape_name =
  let loc = !Ast_helper.default_loc in
  let wrap z =
    match result_wrapper with
    | None -> [%expr fun xml_arg0 -> [%e z]]
    | Some result_wrapper ->
      context_is_referenced := true;
      [%expr
        fun t ->
          let xml_arg0 =
            Xml.child_exn ~context:context_ t [%e Ast_convenience.str result_wrapper]
          in
          [%e z]]
  in
  function
  | Botodata.Boolean_shape _ ->
    [%expr fun xml_arg0 -> Bool.of_string (string_of_xml ~kind:"a boolean" xml_arg0)]
  | Long_shape _ ->
    [%expr fun xml_arg0 -> Int64.of_string (string_of_xml ~kind:"a long" xml_arg0)]
  | Double_shape _ ->
    [%expr fun xml_arg0 -> Float.of_string (string_of_xml ~kind:"a double" xml_arg0)]
  | Integer_shape _ ->
    let kind = sprintf "an integer for %s" shape_name in
    [%expr
      fun xml_arg0 ->
        Int.of_string (string_of_xml ~kind:[%e Ast_convenience.str kind] xml_arg0)]
  | Float_shape _ ->
    [%expr fun xml_arg0 -> Float.of_string (string_of_xml ~kind:"a float" xml_arg0)]
  | Blob_shape _ -> [%expr fun xml_arg0 -> string_of_xml ~kind:"a blob" xml_arg0]
  | Timestamp_shape _ -> [%expr string_of_xml ~kind:"a timestamp"]
  | String_shape _ ->
    context_is_referenced := true;
    [%expr Xml.string_data_exn ~context:context_]
  | Structure_shape { members = []; _ } -> [%expr fun _ -> make ()]
  | Structure_shape ss ->
    let arguments, bindings =
      List.fold_left
        ss.members
        ~init:([ Nolabel, [%expr ()] ], Fn.id)
        ~f:(fun (labels, bindings) (field_name, member) ->
          let argument_is_flattened_list =
            match
              List.Assoc.find_exn service.Botodata.shapes member.shape ~equal:String.equal
            with
            | Botodata.List_shape { flattened = Some b; _ } -> b
            | (_ : Botodata.shape) -> false
          in
          let argument_is_required = Shape.structure_shape_required_field ss field_name in
          let tag_name =
            Option.value
              ~default:field_name
              (match argument_is_flattened_list with
               | false -> member.locationName
               | true -> (
                 match
                   List.Assoc.find_exn service.shapes member.shape ~equal:String.equal
                 with
                 | List_shape { member = { locationName; _ }; _ } ->
                   Option.first_some locationName member.locationName
                 | _ -> assert false))
            |> (* The XML fields in a query protocol body all begin with an *
                  upper case letter *)
            fun field_id ->
            match service.metadata.protocol with
            | `query -> Shape.capitalized_id field_id
            | (_ : Botodata.protocol) -> field_id
          in
          let tag_name_e = Ast_convenience.str tag_name in
          let xml_descendance =
            match argument_is_flattened_list, argument_is_required with
            | false, true ->
              context_is_referenced := true;
              [%expr Xml.child_exn ~context:context_ xml_arg0 [%e tag_name_e]]
            | false, false -> [%expr Xml.child xml_arg0 [%e tag_name_e]]
            | true, true -> [%expr Xml.children xml_arg0 [%e tag_name_e]]
            | true, false -> [%expr Some (Xml.children xml_arg0 [%e tag_name_e])]
          in
          let constructor =
            let of_xml_e =
              Ast_convenience.evar (Shape.capitalized_id member.shape ^ ".of_xml")
            in
            if argument_is_required then of_xml_e else [%expr Option.map ~f:[%e of_xml_e]]
          in
          let arg_id = Shape.uncapitalized_id field_name in
          let label = if argument_is_required then Labelled arg_id else Optional arg_id in
          let pat = Ast_helper.Pat.var (Ast_convenience.mknoloc arg_id) in
          let arg e =
            [%expr
              let [%p pat] = [%e Ast_convenience.app constructor [ xml_descendance ]] in
              [%e bindings e]]
          in
          (label, Ast_convenience.evar arg_id) :: labels, arg)
    in
    wrap (Ast_helper.Exp.apply [%expr make] arguments |> bindings)
  | List_shape ls ->
    let constructor =
      Ast_convenience.evar (Shape.capitalized_id ls.member.shape ^ ".of_xml")
    in
    let nodes_e =
      match ls.flattened with
      | Some true -> [%expr x]
      | Some false | None ->
        [%expr
          Xml.all_children x
          |> List.filter ~f:(function
               | `Data s -> (
                 match Stdlib.String.trim s with
                 | "" -> false
                 | _ -> true)
               | _ -> true)]
    in
    [%expr fun x -> make (List.map [%e nodes_e] ~f:[%e constructor])]
  | Enum_shape _ ->
    let kind = Ast_convenience.str (sprintf "enumeration %s" shape_name) in
    [%expr fun xml_arg0 -> of_string (string_of_xml ~kind:[%e kind] xml_arg0)]
  | Map_shape _ ->
    (* FIXME: unimplemented *)
    [%expr fun _ -> failwith "of_xml_converter_of_shape: Map_shape case not implemented"]
;;

let of_json_converter_of_shape shape_name shape =
  let loc = !Ast_helper.default_loc in
  match shape with
  | Botodata.Boolean_shape _ -> [%expr bool_of_json]
  | Long_shape _ -> [%expr fun j -> Int64.of_float (float_of_json ~kind:"a long" j)]
  | Double_shape _ -> [%expr fun j -> float_of_json ~kind:"a double" j]
  | Integer_shape _ -> [%expr fun j -> Int.of_float (float_of_json ~kind:"an integer" j)]
  | Float_shape _ -> [%expr fun j -> float_of_json ~kind:"a float" j]
  | Blob_shape _ -> [%expr fun j -> string_of_json ~kind:"a blob" j]
  | Timestamp_shape _ -> [%expr timestamp_of_json]
  | String_shape _ ->
    [%expr fun j -> string_of_json ~kind:[%e Ast_convenience.str shape_name] j]
  | Structure_shape ss ->
    let arguments, bindings =
      List.fold_left
        ss.members
        ~init:([ Nolabel, Ast_convenience.unit () ], Fn.id)
        ~f:(fun (labels, bindings) (field_name, member) ->
          let field_name_e = Ast_convenience.str field_name in
          let argument_is_required = Shape.structure_shape_required_field ss field_name in
          let of_json_e =
            Ast_convenience.evar (Shape.capitalized_id member.shape ^ ".of_json")
          in
          let field_map_e =
            if argument_is_required
            then [%expr Awsm.Json.Util.field_map_exn]
            else [%expr Awsm.Json.Util.field_map]
          in
          let arg_id = Shape.uncapitalized_id field_name in
          let pat = Ast_helper.Pat.var (Ast_convenience.mknoloc arg_id) in
          let arg e =
            [%expr
              let [%p pat] = [%e field_map_e] json [%e field_name_e] [%e of_json_e] in
              [%e bindings e]]
          in
          let label = if argument_is_required then Labelled arg_id else Optional arg_id in
          (label, Ast_convenience.evar arg_id) :: labels, arg)
    in
    Ast_convenience.lam
      (Ast_convenience.pvar (if List.is_empty ss.members then "_" else "json"))
      (Ast_helper.Exp.apply (Ast_convenience.evar "make") arguments |> bindings)
  | List_shape ls ->
    let constructor =
      Ast_convenience.evar (Shape.capitalized_id ls.member.shape ^ ".of_json")
    in
    [%expr
      fun j ->
        list_of_json ~kind:[%e Ast_convenience.str shape_name] ~of_json:[%e constructor] j]
  | Enum_shape _ ->
    [%expr
      fun j -> of_string (string_of_json ~kind:[%e Ast_convenience.str shape_name] j)]
  | Map_shape ms ->
    let key_conv = Ast_convenience.evar (Shape.capitalized_id ms.key ^ ".of_string") in
    let value_conv = Ast_convenience.evar (Shape.capitalized_id ms.value ^ ".of_json") in
    [%expr
      fun j -> object_of_json ~key_of_string:[%e key_conv] ~of_json:[%e value_conv] j]
;;

let to_json_converter_of_shape shape =
  let loc = !Ast_helper.default_loc in
  match shape with
  | Botodata.Structure_shape _ | List_shape _ | Map_shape _ ->
    [%expr fun v -> composed_to_json to_value v]
  | Boolean_shape _
  | Enum_shape _
  | Long_shape _
  | Integer_shape _
  | Float_shape _
  | Double_shape _
  | String_shape _
  | Blob_shape _
  | Timestamp_shape _ -> [%expr simple_to_json to_value]
;;

let structure_of_shape (service : Botodata.service) sn shape =
  (* eprintf "debug: structure_of_shape: %s\n" sn; *)
  let loc = !Ast_helper.default_loc in
  let result_wrapper =
    List.find_map service.operations ~f:(fun (operation : Botodata.operation) ->
      Option.bind operation.output ~f:(fun (output : Botodata.operation_output) ->
        if String.equal output.shape sn then output.resultWrapper else None))
  in
  let sig_rec_flag =
    match result_wrapper with
    | Some _ -> Recursive
    | None -> Nonrecursive
  in
  let type_declarations =
    Ast_helper.Str.type_
      sig_rec_flag
      (Type_decl.type_declarations_of_shape ?result_wrapper shape)
  in
  let error_declarations, error_functions =
    match
      List.find_map service.operations ~f:(fun (operation : Botodata.operation) ->
        match operation.output with
        | None -> None
        | Some output -> (
          match String.( = ) sn output.shape with
          | false -> None
          | true ->
            let errors =
              match operation.errors with
              | None -> []
              | Some errors ->
                List.map errors ~f:(fun error -> error.shape)
                (* 2022-07-20 mbac: many AWS services in botocore accidentally list some errors
                     twice.  We used to patch the boto files but that is becoming more labor than
                     teaching our code to dedup them. *)
                |> List.dedup_and_sort ~compare:String.compare
            in
            let funs =
              [ Some (make_error_of_json errors); Some (make_error_of_xml errors) ]
            in
            Some (Type_decl.type_declaration_of_errors operation, funs)))
    with
    | None -> None, None
    | Some (decl, funs) -> Some (Ast_helper.Str.type_ sig_rec_flag [ decl ]), Some funs
  in
  (* Calling this context_ instead of context so it doesn't clash with fields
     that may be named context. *)
  let context_value = [%stri let context_ = [%e Ast_convenience.str sn]] in
  let context_is_referenced = ref false in
  let make_function = Shape_structure.structure_item_of_shape ?result_wrapper shape in
  let opt_to_string_function =
    match shape with
    | Enum_shape es ->
      Some
        (Ast_helper.Str.value
           Nonrecursive
           [ Ast_helper.Vb.mk
               (Ast_helper.Pat.var (Ast_convenience.mknoloc "to_string"))
               (to_string_converter_of_enum_shape es)
           ])
    | _ -> None
  in
  let opt_of_string_function =
    match shape with
    | Boolean_shape _
    | Long_shape _
    | Double_shape _
    | Integer_shape _
    | Float_shape _
    | Blob_shape _
    | Timestamp_shape _
    | String_shape _
    | Enum_shape _ ->
      Some
        (Ast_helper.Str.value
           Nonrecursive
           [ Ast_helper.Vb.mk
               (Ast_helper.Pat.var (Ast_convenience.mknoloc "of_string"))
               (of_string_converter_of_shape shape)
           ])
    | List_shape _ | Structure_shape _ | Map_shape _ -> None
  in
  let opt_of_header_or_of_header_and_body_function =
    match shape with
    | Structure_shape ss ->
      Option.some_if
        (Shape.shape_is_header_structure service shape)
        (Ast_helper.Str.value
           Nonrecursive
           [ Ast_helper.Vb.mk
               (Ast_helper.Pat.var (Ast_convenience.mknoloc "of_header_and_body"))
               (of_header_converter_of_structure_shape service ss)
           ])
    | Map_shape ms ->
      Some
        (Ast_helper.Str.value
           Nonrecursive
           [ Ast_helper.Vb.mk
               (Ast_helper.Pat.var (Ast_convenience.mknoloc "of_header"))
               (of_header_converter_of_map_shape service ms)
           ])
    | _ -> None
  in
  let to_value_function =
    Ast_helper.Str.value
      Nonrecursive
      [ Ast_helper.Vb.mk
          (Ast_helper.Pat.var (Ast_convenience.mknoloc "to_value"))
          (to_value_converter_of_shape ?result_wrapper shape)
      ]
  in
  let to_query_function =
    Ast_helper.Str.value
      Nonrecursive
      [ Ast_helper.Vb.mk
          (Ast_helper.Pat.var (Ast_convenience.mknoloc "to_query"))
          (to_query ())
      ]
  in
  let opt_to_header_function =
    Option.map (to_header_converter_of_shape shape) ~f:(fun x ->
      Ast_helper.Str.value
        Nonrecursive
        [ Ast_helper.Vb.mk (Ast_helper.Pat.var (Ast_convenience.mknoloc "to_header")) x ])
  in
  let of_xml_function =
    Ast_helper.Str.value
      Nonrecursive
      [ Ast_helper.Vb.mk
          (Ast_helper.Pat.var (Ast_convenience.mknoloc "of_xml"))
          (of_xml_converter_of_shape
             ?result_wrapper
             context_is_referenced
             service
             sn
             shape)
      ]
  in
  let of_json_function =
    Ast_helper.Str.value
      Nonrecursive
      [ Ast_helper.Vb.mk
          (Ast_helper.Pat.var (Ast_convenience.mknoloc "of_json"))
          (of_json_converter_of_shape sn shape)
      ]
  in
  let to_json_function =
    Ast_helper.Str.value
      Nonrecursive
      [ Ast_helper.Vb.mk
          (Ast_helper.Pat.var (Ast_convenience.mknoloc "to_json"))
          (to_json_converter_of_shape shape)
      ]
  in
  Ast_helper.Mod.mk
    (Pmod_structure
       (List.filter_opt
          ([ Some type_declarations
           ; error_declarations
           ; (if !context_is_referenced then Some context_value else None)
           ; Some make_function
           ]
          @ Option.value ~default:[] error_functions
          @ [ opt_to_string_function
            ; opt_of_string_function
            ; opt_of_header_or_of_header_and_body_function
            ; Some to_value_function
            ; Some to_query_function
            ; opt_to_header_function
            ; Some of_xml_function
            ; Some of_json_function
            ; Some to_json_function
            ])))
;;

let simple_module_type shape =
  let type_of_shape shape =
    let loc = !Ast_helper.default_loc in
    match shape with
    | Botodata.Boolean_shape _ -> [%type: bool]
    | Float_shape _ | Double_shape _ -> [%type: float]
    | Integer_shape _ -> [%type: int]
    | Long_shape _ -> [%type: int64]
    | String_shape _ | Timestamp_shape _ -> [%type: string]
    | Blob_shape _ -> [%type: string]
    | Map_shape _ | Structure_shape _ | Enum_shape _ | List_shape _ -> assert false
  in
  (* FIXME: This might be deadcode but also not trivial to delete. See details at:
     https://app.asana.com/0/1201896446569636/1203210874139418/f. *)
  let module_type_with (sh : Botodata.shape) (sig_ : string) : Parsetree.module_type =
    let sig_ = Ast_convenience.mknoloc (Longident.Lident sig_) in
    let ty = Ast_helper.Type.mk (Ast_convenience.mknoloc "ty") in
    let ty = { ty with ptype_manifest = Some (type_of_shape sh) } in
    let io = Ast_helper.Type.mk (Ast_convenience.mknoloc "'a io") in
    let longident_loc ty =
      { txt = Longident.Lident ty.ptype_name.txt; loc = ty.ptype_name.loc }
    in
    Ast_helper.Mty.with_
      (Ast_helper.Mty.ident sig_)
      [ Pwith_typesubst (longident_loc ty, ty); Pwith_typesubst (longident_loc io, io) ]
  in
  module_type_with shape "Data.S"
;;

let composed_module_type (service : Botodata.service) sn shape =
  let loc = !Ast_helper.default_loc in
  let result_wrapper =
    List.find_map service.operations ~f:(fun (operation : Botodata.operation) ->
      Option.bind operation.output ~f:(fun (output : Botodata.operation_output) ->
        if String.equal output.shape sn then output.resultWrapper else None))
  in
  let priv = Shape_structure.private_flag_of_shape shape in
  let sig_rec_flag =
    match result_wrapper with
    | Some _ -> Recursive
    | None -> Nonrecursive
  in
  Ast_helper.Mty.signature
    ([ Ast_helper.Sig.type_
         sig_rec_flag
         (Type_decl.type_declarations_of_shape ?result_wrapper ~priv shape)
     ]
    @ [%sig:
        val make : [%t Shape_structure.type_of_shape shape]
        val to_value : t -> Botodata.value
        val to_query : t -> Client.Query.t

        val of_xml
          :  [%t Shape_signature.xml_ty shape]
          -> [%t Shape_signature.of_xml_return_ty shape]

        val of_json : [%t Shape_signature.of_json_arg_ty shape] -> t
        val to_json : t -> [%t Shape_signature.to_json_return_ty shape]]
    @ List.filter_opt
        [ (match shape with
           | Structure_shape _ ->
             (* FIXME(ashish): Is this dead code. In PR #315, I removed
                Future.Pipe.Reader.t from below. However, leaving it in had no
                consequence. *)
             Option.some_if
               (Shape.shape_is_header_structure service shape)
               [%sigi:
                 val of_header_and_body : (string, string) List.Assoc.t * string -> t]
           | Map_shape ms ->
             let module_t s =
               let lid = ksprintf Ast_convenience.lid "%s.t" s in
               Ast_helper.Typ.constr lid []
             in
             let return_type =
               [%type: ([%t module_t ms.key], [%t module_t ms.value]) List.Assoc.t]
             in
             Some
               [%sigi: val of_header : (string, string) List.Assoc.t -> [%t return_type]]
           | (_ : Botodata.shape) -> None)
        ; (match shape with
           | String_shape _
           | Enum_shape _
           | Boolean_shape _
           | Timestamp_shape _
           | Long_shape _
           | Integer_shape _ -> Some [%sigi: val of_string : string -> t]
           | Double_shape _
           | Float_shape _
           | Blob_shape _
           | List_shape _
           | Structure_shape _
           | Map_shape _ -> None)
        ; Option.map (to_header_converter_of_shape shape) ~f:(fun _ ->
            [%sigi: val to_header : t -> string])
        ])
;;

let module_type service sn shape =
  match shape with
  | Botodata.Boolean_shape _
  | Long_shape _
  | Integer_shape _
  | Float_shape _
  | Double_shape _
  | String_shape _
  | Blob_shape _
  | Timestamp_shape _ -> simple_module_type shape
  | Structure_shape _ | Enum_shape _ | Map_shape _ | List_shape _ ->
    composed_module_type service sn shape
;;

let module_binding service sn shape =
  Ast_helper.Str.module_
    (Ast_helper.Mb.mk
       (Ast_convenience.mknoloc (Some (Shape.capitalized_id sn)))
       (structure_of_shape service sn shape))
;;

let string_of_protocol = function
  | `ec2 -> "ec2"
  | `json -> "json"
  | `querystring -> "querystring"
  | `query -> "query"
  | `rest_json -> "rest_json"
  | `rest_xml -> "rest_xml"
;;

let constants_of_service ~awsm_service_id (s : Botodata.service) =
  let opt_item o f = Option.map o ~f in
  let loc = !Ast_helper.default_loc in
  List.filter_opt
    [ Some [%stri [@@@warning "-32"]]
    ; Some [%stri let service = [%e Botocore_service.to_ocaml awsm_service_id]]
    ; Some [%stri let apiVersion = [%e Ast_convenience.str s.metadata.apiVersion]]
    ; Some [%stri let endpointPrefix = [%e Ast_convenience.str s.metadata.endpointPrefix]]
    ; Some
        [%stri let serviceFullName = [%e Ast_convenience.str s.metadata.serviceFullName]]
    ; Some
        [%stri
          let signatureVersion = [%e Ast_convenience.str s.metadata.signatureVersion]]
    ; Some
        [%stri
          let protocol = [%e Ast_convenience.str (string_of_protocol s.metadata.protocol)]]
    ; Some [%stri let globalEndpoint = endpointPrefix ^ ".amazonaws.com"]
    ; opt_item s.metadata.serviceAbbreviation (fun s ->
        [%stri let serviceAbbreviation = [%e Ast_convenience.str s]])
    ; opt_item s.metadata.xmlNamespace (fun uri ->
        [%stri let xmlNamespace = [%e Ast_convenience.str (Uri.to_string uri)]])
    ; opt_item s.metadata.targetPrefix (fun uri ->
        [%stri let targetPrefix = [%e Ast_convenience.str (Uri.to_string uri)]])
    ]
;;

let module_declarations (service : Botodata.service) sg =
  if Shape.Graph.Dfs.has_cycle sg
  then (
    let on_vertex sn ~with_sig acc =
      let shape = List.Assoc.find_exn service.shapes sn ~equal:String.equal in
      let module_ =
        Ast_helper.Mb.mk
          (Ast_convenience.mknoloc (Some (Shape.capitalized_id sn)))
          (if with_sig
          then
            Ast_helper.Mod.mk
              (Pmod_constraint
                 (structure_of_shape service sn shape, module_type service sn shape))
          else structure_of_shape service sn shape)
      in
      module_ :: acc
    in
    let on_scc acc = function
      | [ m ] -> Ast_helper.Str.module_ m :: acc
      | modules -> Ast_helper.Str.rec_module modules :: acc
    in
    let init_scc = [] in
    let init_vertex = [] in
    Shape.Graph.Components.fold ~on_scc ~on_vertex ~init_scc ~init_vertex sg |> List.rev)
  else
    Shape.Graph.Topological.fold
      (fun sn acc ->
        let shape = List.Assoc.find_exn service.shapes sn ~equal:String.equal in
        module_binding service sn shape :: acc)
      sg
      []
;;

let shape_modules service =
  let sg = Shape.Graph.of_service service in
  module_declarations service sg
;;
