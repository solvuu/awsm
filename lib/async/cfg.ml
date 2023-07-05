open! Import

let get ?profile ?aws_access_key_id ?aws_secret_access_key ?region ?output () =
  let profile : string option =
    List.reduce_exn ~f:Option.first_some [ profile; Sys.getenv "AWS_DEFAULT_PROFILE" ]
  in
  let file path of_string =
    match path () with
    | None -> return (Ok None)
    | Some file -> (
      let%bind contents = Reader.file_contents file in
      match of_string contents with
      | Error e -> return (Error e)
      | Ok r -> return (Ok (Some (file, r))))
  in
  match%bind file Awsm.Cfg.Config_file.path Awsm.Cfg.Config_file.of_string with
  | Error e -> return (Error e)
  | Ok config_file -> (
    match%map
      file
        Awsm.Cfg.Shared_credentials_file.path
        Awsm.Cfg.Shared_credentials_file.of_string
    with
    | Error e -> Error e
    | Ok shared_credentials_file ->
      Awsm.Cfg.make
        ?config_file
        ?shared_credentials_file
        ?profile
        ?aws_access_key_id
        ?aws_secret_access_key
        ?region
        ?output
        ())
;;

let get_exn ?profile ?aws_access_key_id ?aws_secret_access_key ?region ?output () =
  match%map get ?profile ?aws_access_key_id ?aws_secret_access_key ?region ?output () with
  | Ok r -> r
  | Error e -> failwithf "Cfg.get_exn: %s" e ()
;;
