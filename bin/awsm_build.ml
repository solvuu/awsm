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

(** Run [prog] on [args] and print any errors to stderr. If the process exits
    successfully, return Ok with the captured stdout, else return Error. *)
let run_and_print_errors ~prog ~args : (string, unit) result =
  let cmdline = sprintf "%s %s" prog (args |> String.concat ~sep:" ") in
  let subcommand_result = Process.run ~prog ~args in
  match subcommand_result with
  | Ok { Process.Output.exit_status; stdout; stderr } -> (
    let () = if String.( <> ) stderr "" then eprintf "%s\n" stderr in
    match exit_status with
    | Ok () -> Ok stdout
    | Error _ as exit_or_signal ->
      eprintf
        "The subcommand '%s' failed: %s\n"
        cmdline
        (Core_unix.Exit_or_signal.to_string_hum exit_or_signal);
      Error ())
  | Error exn ->
    eprintf "Process.run raised an exception: %s: %s\n" cmdline (Exn.to_string exn);
    Error ()
;;

let ocamlformat ?(prog = "ocamlformat") ~path () =
  run_and_print_errors ~prog ~args:[ "--"; path ]
;;

let format_dune_file ~path =
  run_and_print_errors ~prog:"dune" ~args:[ "format-dune-file"; path ]
;;

let save_file_and_format (fmt : [ `dune | `ocaml ]) ~(path : string) ~(contents : string)
  : unit
  =
  Out_channel.write_all path ~data:contents;
  let result =
    match fmt with
    | `ocaml -> ocamlformat ~path ()
    | `dune -> format_dune_file ~path
  in
  let contents2 =
    match result with
    | Ok x -> x
    | Error () -> exit 1
  in
  match String.equal contents contents2 with
  | true -> ()
  | false -> Out_channel.write_all path ~data:contents2
;;

let dashes_to_underscores : string -> string =
  String.map ~f:(function
    | '-' -> '_'
    | c -> c)
;;

(** Services that we don't yet support for some reason. *)
let unsupported_services : String.Set.t =
  String.Set.of_list
    [ "apigateway"
    ; "apigatewayv2"
    ; "appconfig"
    ; "appconfigdata"
    ; "appsync"
    ; "cloudhsm"
    ; "codeguruprofiler"
    ; "dataexchange"
    ; "health"
    ; "iottwinmaker"
    ; "lex-runtime"
    ; "lexv2-runtime"
    ; "lookoutvision"
    ; "mediaconnect"
    ; "medialive"
    ; "mq"
    ; "pinpoint"
    ; "s3control"
    ; "sagemaker-runtime"
    ; "workmailmessageflow"
    ]
;;

let get_all_services ~(botocore_data : string) : string list =
  Sys_unix.ls_dir botocore_data
  |> List.filter ~f:(fun ent -> Sys_unix.is_directory_exn (botocore_data ^/ ent))
  |> List.sort ~compare:String.compare
;;

module Param = struct
  let botocore_data : string Command.Param.t =
    Command.Param.(
      flag
        "--botocore-data"
        (required string)
        ~doc:"PATH Path to the botocore/data directory.")
  ;;
end

module Build_dune_project_file : sig
  val main : Command.t
end = struct
  let header () : string =
    let async_base_pkg =
      {|
    (package
      (name awsm-async)
      (synopsis "AWS API base library Async")
      (depends
        awsm
        cohttp-async
        (async
          (>= v0.11.0))
        async_ssl))
      |}
    in
    let async_srp_pkg =
      {|
    (package
      (name awsm-srp-async)
      (synopsis "AWS Async API for SRP")
      (depends awsm-async awsm-cognito-idp-async awsm-srp))
      |}
    in
    let lwt_base_pkg =
      {|
    (package
      (name awsm-lwt)
      (synopsis "AWS API base library Lwt")
      (depends awsm lwt cohttp-lwt-unix))
      |}
    in
    [%string
      {|
    (lang dune 3.6)
    ;; dune-project is auto-generated, see bin/awsm_build.ml
    (name awsm)

    ; generating the opam files pollutes the top-level directory too much
    ; try doing without, here, until this is merged
    ; https://github.com/ocaml/dune/issues/6205

    (generate_opam_files true)

    ; just a placeholder to make warnings go away; this isn't actually
    ; BSD licensed

    (license BSD-3-Clause)

    (authors "Solvuu")

    (maintainers "Ashish Agarwal <ashish@solvuu.com>")

    (homepage "https://github.com/solvuu/awsm")

    (source
      (github solvuu/awsm))

    (package
      (name awsm)
      (synopsis "AWS API base library")
      (depends
        awsm-codegen
        (base64 (>= 3.1.0))
        (cohttp (>= 0.21.0))
        cryptokit
        xmlm
        zarith))

    %{async_base_pkg}

    %{lwt_base_pkg}

    (package
      (name awsm-codegen)
      (synopsis "AWS botocore code generator")
      (depends
        cohttp
        ocamlgraph
        ppxlib
        (re (>= 1.7.2))
        (sedlex (>= 2.3))
        xmlm
        yojson))

    (package
      (name awsm-unix)
      (synopsis "AWS API base library Unix"))

    ;; Packages above this line are base packages needed by all services.

    ;; The following SRP packages provide support for Cognito SRP.
    (package
      (name awsm-srp)
      (synopsis "AWS API for SRP")
      (depends awsm))

    %{async_srp_pkg}

    ;; Packages after this line each regard a specific service.
      |}]
  ;;

  let make_base_package ~(service : string) : string =
    [%string
      {|
    (package
      (name awsm-%{service})
      (synopsis "AWS API for %{String.capitalize service}")
      (depends awsm))
        |}]
  ;;

  let make_io_package ~service (io_kind : [ `async | `lwt ]) =
    let c_service = String.capitalize service in
    let io =
      match io_kind with
      | `async -> "async"
      | `lwt -> "lwt"
    in
    let io_cap = String.capitalize io in
    [%string
      {|
    (package
    (name awsm-%{service}-%{io})
    (synopsis "AWS %{io_cap} API for %{c_service}")
    (depends awsm-%{io} awsm-%{service}))
        |}]
  ;;

  let make services =
    services
    |> List.map ~f:(fun service ->
         [ make_base_package ~service
         ; make_io_package ~service `async
           (* FIXME: Build lwt packages too once the opam files can be put under opam/. *)
           (* ; make_io_package ~service `lwt *)
         ])
    |> List.concat
    |> (fun l -> header () :: l)
    |> String.concat ~sep:"\n"
  ;;

  let main : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate the dune-project file"
      [%map_open
        let botocore_data = Param.botocore_data in
        fun () ->
          let all_services = get_all_services ~botocore_data in
          let generated_services =
            List.filter all_services ~f:(fun service ->
              not (Set.mem unsupported_services service))
          in
          let contents = make generated_services in
          save_file_and_format `dune ~path:"dune-project" ~contents]
  ;;
end

module Build_service_module : sig
  val main : Command.t
end = struct
  let make_ml services =
    let b = Buffer.create 1024 in
    bprintf
      b
      {|(* do not edit! module generated by bin/awsm_build.ml *)
    open! Core
    open! Import

    type t = string [@@deriving sexp, compare]

    let equal = String.equal

    let of_string (x : string) : t =
    match x with
    |};
    List.iter services ~f:(fun s -> bprintf b "  | \"%s\"\n" s);
    bprintf b "    -> x\n";
    bprintf b "  | _ -> failwithf \"unknown service: %%s\" x ()\n";
    bprintf b ";;\n";
    bprintf b "let to_string (x : t) = x\n";
    bprintf b "let all : t list = [\n";
    List.iter services ~f:(fun s -> bprintf b " \"%s\";\n" s);
    bprintf b "]\n";
    bprintf b ";;\n";
    List.iter services ~f:(fun s ->
      bprintf b "let %s = \"%s\"\n" (dashes_to_underscores s) s);
    Buffer.contents b
  ;;

  let make_mli services =
    let b = Buffer.create 4096 in
    bprintf
      b
      {|(* do not edit! module generated by bin/awsm_build.ml *)
    open! Core
    open! Import

    type t = private string [@@deriving sexp, compare]

    val equal: t -> t -> bool
    val of_string: string -> t
    val to_string: t -> string
    val all: t list
    |};
    List.iter services ~f:(fun s -> bprintf b "val %s : t\n" (dashes_to_underscores s));
    Buffer.contents b
  ;;

  let main : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate the Service module"
      ~readme:(fun () ->
        "Print a service.ml and service.mli file to the current working directory.\n\
         Existing files of this name will be overwritten.\n\n\
         Note this generator is not provided in the main awsm-codegen CLI because the\n\
         Service module is needed in the awsm-codegen library. Thus, that would lead to\n\
         a circular dependency.")
      [%map_open
        let botocore_data = Param.botocore_data in
        fun () ->
          let all_services = get_all_services ~botocore_data in
          save_file_and_format `ocaml ~path:"service.ml" ~contents:(make_ml all_services);
          save_file_and_format
            `ocaml
            ~path:"service.mli"
            ~contents:(make_mli all_services)]
  ;;
end

let main : Command.t =
  Command.group
    ~summary:"scripts used to build awsm"
    ~readme:(fun () -> "Scripts used internally to build the awsm project.")
    [ "build-dune-project", Build_dune_project_file.main
    ; "build-service-module", Build_service_module.main
    ]
;;

let () =
  try Command_unix.run main with
  | e -> eprintf "%s\n" (Exn.to_string e)
;;
