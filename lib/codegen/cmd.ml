open! Core
open! Import

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

let latest_date ~botocore_data ~service =
  let dir = botocore_data ^/ service in
  match Sys_unix.is_directory_exn dir with
  | false -> Error `Unknown_directory
  | true ->
    let date =
      dir
      |> Sys_unix.ls_dir
      |> List.sort ~compare:String.compare
      |> List.rev
      |> List.hd_exn
    in
    Ok date
;;

module Param = struct
  let service_name ?(flag = "service") () =
    let name = sprintf "--%s" flag in
    Command.Param.(
      flag
        name
        (required string)
        ~doc:
          "STRING Name of a service. Should match a subdirectory name under \
           botocore/data.")
  ;;

  let service_date =
    Command.Param.(
      flag
        "--service-date"
        (required string)
        ~doc:
          "YYYY-MM Service date. Should match a subdirectory name under the respective \
           service in botocore/data.")
  ;;

  let botocore_data =
    Command.Param.(
      flag
        "--botocore-data"
        (required string)
        ~doc:"DIR Path to the botocore/data directory.")
  ;;

  let service_names =
    Command.Param.(
      flag
        "--services"
        (optional string)
        ~doc:
          "NAMES Comma separated list of services. Default: all services (except those \
           not currently supported).")
  ;;

  let botocore_data_and_service_names : (string * string list) Command.Param.t =
    let make botocore_data services =
      let services =
        match services with
        | Some x -> x |> String.split ~on:',' |> List.map ~f:String.strip
        | None ->
          let all = get_all_services ~botocore_data |> String.Set.of_list in
          let unsupported = unsupported_services in
          Set.diff all unsupported |> Set.to_list
      in
      botocore_data, services
    in
    let open Command.Param.Applicative_infix in
    Command.Param.return make <*> botocore_data <*> service_names
  ;;

  let endpoints_json_file =
    Command.Param.(
      flag
        "--endpoints"
        (required string)
        ~doc:"PATH Path to the botocore/data/endpoints.json file.")
  ;;

  let service_json_file =
    Command.Param.(
      flag
        "--service"
        (required string)
        ~doc:"PATH Path to a service*.json file within botocore.")
  ;;

  let outdir = Command.Param.(flag "-o" (required string) ~doc:"DIR Output directory.")

  let impl =
    Command.Param.(
      flag
        "--impl"
        (required string)
        ~doc:"PATH Path where the .ml file should be written.")
  ;;

  let intf =
    Command.Param.(
      flag
        "--intf"
        (required string)
        ~doc:"PATH Path where the .mli file should be written.")
  ;;

  let submodules =
    Command.Param.(
      flag
        "--sub"
        (listed string)
        ~doc:"Names of sub-modules (for breaking up huge module compilation)")
  ;;

  let io_subsystem =
    Command.Param.(flag "--io-subsystem" (required string) ~doc:"SUBSYS async or lwt")
    |> Command.Param.map ~f:(function
         | "async" -> `Async
         | "lwt" -> `Lwt
         | fmt -> failwithf "Unknown io-subsystem: %s" fmt ())
  ;;

  let base_module =
    Command.Param.(
      flag
        "--base-module"
        (required string)
        ~doc:"In which module Endpoints & Values are searched")
  ;;
end

let botocore_endpoints : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"generate Botocore_endpoints module"
    [%map_open
      let file = Param.endpoints_json_file in
      fun () ->
        let endpoints = file |> In_channel.read_all |> Botocore_endpoints.of_json in
        let structure =
          [ Botocore_endpoints.make_lookup_uri endpoints
          ; Botocore_endpoints.make_lookup_credential_scope endpoints
          ]
        in
        print_endline (Util.structure_to_string structure)]
;;

module Service = struct
  let dune : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a service's dune file"
      [%map_open
        let service = Param.service_name ()
        and date = Param.service_date in
        fun () -> Dune.make ~date ~service |> print_endline]
  ;;

  let endpoints : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a service's Endpoints module"
      [%map_open
        let service = Param.service_json_file
        and impl = Param.impl in
        fun () ->
          let data =
            service
            |> In_channel.read_all
            |> Botocore_service.of_json
            |> Service_endpoints.make
            |> Util.structure_to_string
          in
          Out_channel.write_all impl ~data]
  ;;

  let values : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a service's Values module"
      [%map_open
        let awsm_service_id = Param.service_name ~flag:"service-id" ()
        and service = Param.service_json_file
        and impl = Param.impl
        and submodules = Param.submodules in
        fun () ->
          let service = service |> In_channel.read_all |> Botocore_service.of_json in
          let main_module, submodules =
            Values.make ~awsm_service_id ~submodules service
          in
          Out_channel.write_all impl ~data:(main_module |> Util.structure_to_string);
          submodules
          |> List.iter ~f:(fun (filename, struct_) ->
               Out_channel.write_all filename ~data:(struct_ |> Util.structure_to_string))]
  ;;

  let main : Command.t =
    Command.group
      ~summary:"generate a service's package"
      ~readme:(fun () ->
        "The dune subcommand generates a dune file that contains rules to generate all\n\
         other files. Thus, you either need to call only the dune subcommand, or all the\n\
         other subcommands, but not both.")
      [ "dune", dune; "endpoints", endpoints; "values", values ]
  ;;
end

module Service_io = struct
  let dune : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a service's dune file"
      [%map_open
        let service = Param.service_name ()
        and date = Param.service_date
        and io_subsystem = Param.io_subsystem in
        fun () -> Dune.make_io io_subsystem ~date ~service |> print_endline]
  ;;

  let values : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"gnerate a service's Values module"
      [%map_open
        let service = Param.service_name () in
        fun () ->
          let service =
            service
            |> String.map ~f:(function
                 | '-' -> '_'
                 | c -> c)
          in
          printf
            {|(* do not edit! generated module *)
    include Awsm_%s.Values|}
            service]
  ;;

  let io : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a service's IO module"
      [%map_open
        let service = Param.service_json_file
        and impl = Param.impl
        and intf = Param.intf
        and base_module = Param.base_module
        and io_subsystem = Param.io_subsystem in
        fun () ->
          let service = service |> In_channel.read_all |> Botocore_service.of_json in
          let endpoints =
            service.operations |> List.map ~f:(Endpoint.of_botodata ~service)
          in
          let () =
            Io.eval_structure ~base_module ~io_subsystem endpoints
            |> Util.structure_to_string
            |> fun data -> Out_channel.write_all impl ~data
          in
          let () =
            Io.eval_signature
              ~protocol:service.metadata.protocol
              ~base_module
              ~io_subsystem
              endpoints
            |> Util.signature_to_string
            |> fun data -> Out_channel.write_all intf ~data
          in
          ()]
  ;;

  let cli : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a service's Cli module"
      ~readme:(fun () ->
        "The generated module depends on Core and Async. Also, this is only provides a\n\
         value of type Command.t. You still need to define an executable by calling\n\
         Command.run. We provide another command for generating such an executable.")
      [%map_open
        let service = Param.service_json_file
        and impl = Param.impl
        and submodules = Param.submodules in
        fun () ->
          let service = service |> In_channel.read_all |> Botocore_service.of_json in
          let main_module, submodules = Cli.make ~submodules service in
          Out_channel.write_all impl ~data:(main_module |> Util.structure_to_string);
          submodules
          |> List.iter ~f:(fun (filename, struct_) ->
               Out_channel.write_all filename ~data:(struct_ |> Util.structure_to_string))]
  ;;

  let main : Command.t =
    Command.group
      ~summary:"generate a service's IO package"
      ~readme:(fun () ->
        "The dune subcommand generates a dune file that contains rules to generate all\n\
         other files. Thus, you either need to call only the dune subcommand, or all the\n\
         other subcommands, but not both.\n\n\
         We currently support the creation of packages for async and lwt. If async is\n\
         selected, we also generate a CLI.")
      [ "dune", dune; "cli", cli; "io", io; "values", values ]
  ;;
end

module Cli = struct
  let dune : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a dune file"
      [%map_open
        let service = Param.service_name () in
        fun () -> Dune.make_cli_async ~service |> print_endline]
  ;;

  let script : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate a script"
      [%map_open
        let service = Param.service_name () in
        fun () ->
          let service =
            service
            |> String.map ~f:(function
                 | '-' -> '_'
                 | c -> c)
          in
          printf
            {|(* do not edit! generated module *)

    let () = Command_unix.run Awsm_%s_async.Cli.main
    |}
            service]
  ;;

  let main : Command.t =
    Command.group
      ~summary:"generate a package providing a service's CLI"
      ~readme:(fun () ->
        "The dune subcommand generates a dune file that contains rules to generate all\n\
         other files. Thus, you either need to call only the dune subcommand, or all the\n\
         other subcommands, but not both.")
      [ "dune", dune; "script", script ]
  ;;
end

module Services = struct
  let main : Command.t =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"generate multiple services' packages"
      [%map_open
        let botocore_data, services = Param.botocore_data_and_service_names
        and outdir = Param.outdir in
        fun () ->
          let temp_file = Filename_unix.temp_file "dune" "" in
          let print_dune_file ~outdir ~data =
            Out_channel.write_all temp_file ~data;
            let prog = "dune" in
            let args = [ "format-dune-file"; temp_file ] in
            match Process.run ~prog ~args with
            | Error exn ->
              failwithf
                "%s %s\n%s"
                prog
                (args |> String.concat ~sep:" ")
                (Exn.to_string exn)
                ()
            | Ok { stdout; exit_status; stderr = _ } -> (
              match exit_status with
              | Error e ->
                failwithf
                  "%s"
                  (e |> Core_unix.Exit_or_signal.sexp_of_error |> Sexp.to_string)
                  ()
              | Ok () ->
                Util.mkdir_exn outdir;
                Out_channel.write_all (outdir ^/ "dune") ~data:stdout)
          in
          let () =
            services
            |> List.iter ~f:(fun service ->
                 match latest_date ~botocore_data ~service with
                 | Error `Unknown_directory ->
                   failwithf "Unknown directory: %s/%s" botocore_data service ()
                 | Ok date ->
                   print_dune_file
                     ~outdir:(outdir ^/ service)
                     ~data:(Dune.make ~date ~service);
                   (* print_dune_file
                   ~outdir:(outdir ^/ service ^ "-lwt")
                   ~data:(Dune.gen_dune_io `Lwt ~date ~service); *)
                   print_dune_file
                     ~outdir:(outdir ^/ service ^ "-async")
                     ~data:(Dune.make_io `Async ~date ~service);
                   print_dune_file
                     ~outdir:(outdir ^/ service ^ "-cli-async")
                     ~data:(Dune.make_cli_async ~service))
          in
          Sys_unix.remove temp_file]
  ;;
end

let main =
  Command.group
    ~summary:"Code generation from boto data"
    [ "botocore-endpoints", botocore_endpoints
    ; "service", Service.main
    ; "service-io", Service_io.main
    ; "services", Services.main
    ; "cli", Cli.main
    ]
;;
