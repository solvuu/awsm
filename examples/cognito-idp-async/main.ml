open Awsm_cognito_idp_async

let () =
  try Command_unix.run Cli.main with
  | e -> eprintf "%s\n" (Exn.to_string e)
;;
