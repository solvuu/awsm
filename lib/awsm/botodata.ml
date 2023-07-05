open! Import
include Awsm_codegen.Botodata

module Json : sig
  val value_to_json_scalar : value -> Json.t
  val value_to_json : value -> Json.t
end = struct
  let value_to_string : value -> string option = function
    | `Blob b -> Some b
    | `Timestamp t -> Some t
    | `String s -> Some s
    | `Enum e -> Some e
    | `Integer i -> Some (Int.to_string i)
    | `Boolean b -> Some (Bool.to_string b)
    | _ -> None
  ;;

  let rec value_to_json_scalar : value -> Json.t = function
    | `Boolean b -> `Bool b
    | `Integer i -> `Int i
    | `Long l -> (
      (* try storing these as ints if possible *)
      try `Int (Core.Int64.to_int_exn l) with
      | Failure _ -> `Intlit (Core.Int64.to_string l))
    | `Float f | `Double f -> `Float f
    | `Blob s | `String s | `Timestamp s | `Enum s -> `String s
    | `Structure fields -> structure_to_json fields
    | `List xs -> list_to_json xs
    | `Map xs -> map_to_json xs

  and map_to_json (xs : (value * value) list) : Json.t =
    `Assoc
      (List.map
         ~f:(fun (k, v) ->
           let k_string =
             value_to_string k
             |> Option.value_exn
                  ~message:"not a valid shape type for a map shape key"
                  ?error:None
                  ?here:None
           in
           let v_value = value_to_json_scalar v in
           k_string, v_value)
         xs)

  and structure_to_json (fields : (string * value) list) : Json.t =
    let f (k, v) = k, value_to_json_scalar v in
    `Assoc (List.map fields ~f)

  and list_to_json (xs : value list) : Json.t =
    `List (List.map xs ~f:value_to_json_scalar)

  and value_to_json : value -> Json.t = function
    | `Map fields -> map_to_json fields
    | `Structure fields -> structure_to_json fields
    | `List xs -> list_to_json xs
    | `Enum _
    | `Boolean _
    | `Long _
    | `Float _
    | `Double _
    | `Integer _
    | `Blob _
    | `Timestamp _
    | `String _ ->
      raise (Invalid_argument "Botodata.Json.value_to_json: expected structure or list")
  ;;
end
