open Awsm_s3_async

let () =
  try Command_unix.run Cli.main with
  | e -> eprintf "%s\n" (Exn.to_string e)
;;
