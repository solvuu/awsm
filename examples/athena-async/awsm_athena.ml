module Cfg = Awsm_async.Cfg

let ls_main () =
  let%bind cfg = Cfg.get_exn () in
  let pipe = Athena.Query.ls ~max_results:10 cfg in
  let%bind lst = Pipe.to_list pipe in
  printf "athena query results:\n";
  List.iter lst ~f:(function `Athena_execution_id x -> printf "execution_id: %s\n" x);
  return ()
;;

let () =
  let group =
    Command.group
      ~summary:"athena demo"
      [ ( "ls"
        , Command.async
            ~summary:"list queries"
            (Command.Param.return (fun () -> ls_main ())) )
      ]
  in
  Command_unix.run group
;;
