open Awsm_ebs_async

let () =
  try Command_unix.run Cli.main with
  | e -> eprintf "%s\n" (Exn.to_string e)
;;
