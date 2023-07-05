open! Core

module Process = struct
  module Output = struct
    type t =
      { exit_status : Core_unix.Exit_or_signal.t
      ; stdout : string
      ; stderr : string
      }
  end

  let run ~prog ~args : (Output.t, exn) result =
    let res =
      Result.try_with (fun () ->
        let read_into_buf =
          let len = 8192 in
          let buf = Bytes.create len in
          fun ~buffer ~fd ->
            match Core_unix.read ~restart:true fd ~buf ~pos:0 ~len with
            | -1 ->
              failwithf
                "Error while reading %d from %s"
                (Core_unix.File_descr.to_int fd)
                (String.concat ~sep:" " (prog :: args))
                ()
            | 0 -> `Eof
            | num_bytes ->
              Buffer.add_subbytes buffer buf ~pos:0 ~len:num_bytes;
              `Read num_bytes
        in
        let process_fd ~fds ~read ~fd ~buffer =
          if not (List.mem read ~equal:Core_unix.File_descr.equal fd)
          then fds
          else (
            match read_into_buf ~buffer ~fd with
            | `Eof ->
              List.filter fds ~f:(fun fd' -> not @@ Core_unix.File_descr.equal fd fd')
            | `Read _n -> fds)
        in
        let process_info = Core_unix.create_process ~prog ~args in
        let unix_close = Core_unix.close ~restart:true in
        let () = unix_close process_info.stdin in
        let stdout, stderr =
          let outbuf = Buffer.create 16 in
          let errbuf = Buffer.create 16 in
          let rec loop fds =
            match
              Core_unix.select
                ~restart:true
                ~read:fds
                ~write:[]
                ~except:[]
                ~timeout:`Never
                ()
            with
            | { read; write = []; except = [] } -> (
              let fds = process_fd ~fds ~read ~fd:process_info.stdout ~buffer:outbuf in
              let fds = process_fd ~fds ~read ~fd:process_info.stderr ~buffer:errbuf in
              match fds with
              | [] -> ()
              | fds -> loop fds)
            | _ ->
              (* This shouldn't happen. We only provide read fds. *)
              assert false
          in
          let () = loop [ process_info.stdout; process_info.stderr ] in
          let () = unix_close process_info.stdout in
          let () = unix_close process_info.stderr in
          Buffer.contents outbuf, Buffer.contents errbuf
        in
        let exit_status = Core_unix.waitpid process_info.pid in
        { Output.exit_status; stdout; stderr })
    in
    match res with
    | Ok c -> Ok c
    | Error e -> Error e
  ;;
end

module Util = struct
  let mkdir_exn (dir : string) : unit =
    match Sys_unix.is_directory dir with
    | `Yes -> ()
    | `Unknown -> failwithf "unable to determine if %s is a directory" dir ()
    | `No -> (
      match Sys_unix.is_file dir with
      | `No -> Core_unix.mkdir dir ~perm:0o755
      | `Unknown -> failwithf "unable to determine if %s is a file" dir ()
      | `Yes ->
        failwithf
          "cannot make directory %s because it already exists as a regular file"
          dir
          ())
  ;;

  let camel_to_snake_case ?(sep = '_') (s : string) : string =
    String.uncapitalize s
    |> String.concat_map ~f:(fun c ->
         if Char.is_uppercase c
         then Printf.sprintf "%c%c" sep (Char.lowercase c)
         else String.of_char c)
  ;;

  let%expect_test "camel_to_snake_case" =
    let test s = print_endline (camel_to_snake_case s) in
    test "AbortMultipartUpload";
    [%expect "abort_multipart_upload"];
    test "CompleteMultipartUpload";
    [%expect "complete_multipart_upload"]
  ;;

  let tokenize (read_token : Sedlexing.lexbuf -> ('a option, 'err) result) (s : string)
    : ('a list, 'err) result
    =
    let lexbuf = Sedlexing.Latin1.from_string s in
    let accum = ref [] in
    let rec loop () =
      match read_token lexbuf with
      | Error _ as e -> e
      | Ok None -> Ok ()
      | Ok (Some tok) ->
        accum := tok :: !accum;
        loop ()
    in
    match loop () with
    | Ok () -> Ok (List.rev !accum)
    | Error _ as e -> e
  ;;

  let to_string_of_printer (f : Format.formatter -> 'a -> unit) : 'a -> string =
   fun x ->
    let buf = Buffer.create 128 in
    let fmt = Format.formatter_of_buffer buf in
    f fmt x;
    Format.pp_print_flush fmt ();
    Buffer.contents buf
 ;;

  let structure_to_string : Parsetree.structure -> string =
    to_string_of_printer Pprintast.structure
  ;;

  let signature_to_string : Parsetree.signature -> string =
    to_string_of_printer Pprintast.signature
  ;;

  let expression_to_string : Parsetree.expression -> string =
    to_string_of_printer Pprintast.expression
  ;;

  let core_type_to_string : Parsetree.core_type -> string =
    to_string_of_printer Pprintast.core_type
  ;;
end
