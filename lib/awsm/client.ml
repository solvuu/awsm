open! Import

module Query = struct
  type t =
    | String of string
    | List of t list
    | Structure of (string * t) list
    | Map of (string * string) list

  type value =
    [ `Blob of string
    | `Boolean of bool
    | `Double of Float.t
    | `Enum of string
    | `Float of Float.t
    | `Integer of int
    | `List of value list
    | `Long of Int64.t
    | `Map of (value * value) list
    | `String of string
    | `Structure of (string * value) list
    | `Timestamp of string
    ]

  let rec of_value = function
    | `Boolean b -> String (string_of_bool b)
    | `Long l -> String (Int64.to_string l)
    | `Float f -> String (Float.to_string f)
    | `Double d -> String (Float.to_string d)
    | `Integer i -> String (string_of_int i)
    | `Blob b -> String b
    | `Timestamp t -> String t
    | `String s -> String s
    | `Structure fields ->
      let f (k, v) = k, of_value v in
      Structure (List.map fields ~f)
    | `List xs -> List (List.map xs ~f:of_value)
    | `Enum e -> String e
    | `Map fields ->
      Map
        (List.map fields ~f:(fun (k, v) ->
           match of_value k, of_value v with
           | String k, String v -> k, v
           | _, _ -> assert false))
  ;;

  let enter base new_level =
    if String.equal base "" then new_level else sprintf "%s.%s" base new_level
  ;;

  let rec render_aux ctx = function
    | String s -> [ ctx, [ s ] ]
    | List xs ->
      List.mapi xs ~f:(fun i q -> render_aux (enter ctx (string_of_int (i + 1))) q)
      |> List.concat
    | Structure fields ->
      List.map fields ~f:(fun (field, q) ->
        let field = String.capitalize field in
        render_aux (enter ctx field) q)
      |> List.concat
    | Map fields ->
      List.mapi fields ~f:(fun i (k, v) ->
        let k = String.capitalize k in
        let i = i + 1 in
        let name = sprintf "%d.Name" i in
        let value = sprintf "%d.Value" i in
        render_aux (enter ctx name) (String k) @ render_aux (enter ctx value) (String v))
      |> List.concat
  ;;

  let render q = render_aux "" q
end

let content_md5 body = body |> Md5.digest_string |> Md5.to_binary |> Base64.encode_string
