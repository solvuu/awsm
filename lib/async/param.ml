(******************************************************************************)
(* Common app parameters                                                      *)
(******************************************************************************)

open! Import

let ( %: ) = Command.Param.( %: )

let log_level =
  Command.Param.flag
    "-log"
    (Command.Param.optional_with_default `Info Log.Level.arg)
    ~doc:"level Log level can be debug, info, or error. Default is info."
;;

let profile =
  Command.Param.flag
    "-profile"
    Command.Param.(optional string)
    ~doc:
      "string Within the config file, settings are chosen from given profile, which is \
       taken as the first one of the following: value of this option if given, \
       environment variable AWS_DEFAULT_PROFILE if set, and finally \"default\"."
;;

let infile = Command.Param.anon ("file" %: Filename_unix.arg_type)

let outfile =
  Command.Param.flag
    "-o"
    (Command.Param.optional Filename_unix.arg_type)
    ~doc:"file Output file. If not provided, output will be written to stdout."
;;

let bucket = Command.Param.anon ("bucket" %: Command.Param.string)
let key = Command.Param.anon ("key" %: Command.Param.string)
let role_session_name = Command.Param.anon ("role-session-name" %: Command.Param.string)
let role_arn = Command.Param.anon ("role-arn" %: Command.Param.string)

let duration_sec =
  Command.Param.flag
    "-duration-sec"
    Command.Param.(optional int)
    ~doc:"int Override default duration of the temporary credentials."
;;

let max_results =
  Command.Param.flag
    "-max-results"
    Command.Param.(optional int)
    ~doc:"int Maximum results returned by list based endpoints."
;;
