open! Import

module Req = struct
  let string_of_file : string -> string =
    let replace_pattern = String.Search_pattern.create "http/1.1" in
    fun filename ->
      In_channel.read_all filename
      |> fun content ->
      String.Search_pattern.replace_first replace_pattern ~in_:content ~with_:"HTTP/1.1"
  ;;

  let request_of_file filename =
    let s = string_of_file filename in
    let s = if String.is_suffix s ~suffix:"\n" then s else s ^ "\n" in
    match Cohttp.Request.of_string s with
    | `Eof -> assert false
    | `Invalid x -> failwith x
    | `Ok x -> x
  ;;

  (** Get body from given filename. Ideally we could use Cohttp's Request parser
      to do this, but it requires a content-length header to parse bodies, which
      AWS's .req files don't have. *)
  let body_of_file filename : Cohttp.Body.t =
    let rec loop body_started accum = function
      | [] -> accum
      | "" :: lines -> (
        match body_started with
        | true -> loop body_started ("" :: accum) lines
        | false -> loop true accum lines)
      | x :: lines -> (
        match body_started with
        | true -> loop body_started (x :: accum) lines
        | false -> loop body_started accum lines)
    in
    In_channel.read_lines filename |> loop false [] |> List.rev |> fun x -> `Strings x
  ;;

  let of_file filename = request_of_file filename, body_of_file filename
end

module Creq = struct
  let of_file : string -> string =
    let replace_pattern = String.Search_pattern.create "\r\n" in
    fun filename ->
      In_channel.read_all filename
      |> fun content ->
      String.Search_pattern.replace_all replace_pattern ~in_:content ~with_:"\n"
  ;;
end
