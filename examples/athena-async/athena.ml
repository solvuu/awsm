module Values = Awsm_athena_async.Values
module Io = Awsm_athena_async.Io

let athena_call = Awsm_async.Http.Io.call ~service:Values.service

let dispatch_exn ~name ~sexp_of_error ~f =
  match%bind f () with
  | Ok v -> return v
  | Error (`AWS err) ->
    failwithf "%s: %s" name (err |> sexp_of_error |> Sexp.to_string_hum) ()
  | Error (`Transport _) -> failwithf "%s: transport error" name ()
;;

module Query = struct
  type query_id_params =
    { execution_id : string
    ; next_token : string option
    }
  [@@deriving sexp]

  type athena_start =
    { result_configuration : Values.ResultConfiguration.t
    ; query_execution_output : Values.StartQueryExecutionOutput.t
    }
  [@@deriving sexp]

  type t =
    [ `Athena_execution_id of string
    | `Athena_start of athena_start
    | `Athena_execution of Values.GetQueryExecutionOutput.t
    ]
  [@@deriving sexp]

  let of_id id : [< t ] = `Athena_execution_id id

  let submit ?idem_potency_token ?output_location ~query_string ~bucket cfg =
    let idem_potency_token =
      match idem_potency_token with
      | Some token -> token
      | None -> Uuid_unix.create () |> Uuid.to_string
    in
    let clientRequestToken = idem_potency_token |> Values.IdempotencyToken.make in
    let result_configuration =
      Values.ResultConfiguration.make
        ~outputLocation:
          (Option.value
             ~default:(sprintf "s3://%s/athena-output/%s" bucket idem_potency_token)
             output_location)
        ()
    in
    dispatch_exn
      ~name:"athena.start_query_execution"
      ~sexp_of_error:Values.StartQueryExecutionOutput.sexp_of_error
      ~f:(fun () ->
      Io.start_query_execution
        (athena_call ~cfg)
        (Values.StartQueryExecutionInput.make
           ~clientRequestToken
           ~queryString:(Values.QueryString.make query_string)
           ?queryExecutionContext:None
           ~resultConfiguration:result_configuration
           ()))
    >>| fun query_execution_output ->
    `Ok (`Athena_start { result_configuration; query_execution_output })
  ;;

  let execution_id : [< t ] -> string option = function
    | `Athena_execution_id id -> Some id
    | `Athena_execution
        { Values.GetQueryExecutionOutput.queryExecution =
            Some { Values.QueryExecution.queryExecutionId; _ }
        } -> queryExecutionId
    | `Athena_start
        { result_configuration = _
        ; query_execution_output = { Values.StartQueryExecutionOutput.queryExecutionId }
        } -> queryExecutionId
    | `Athena_execution { Values.GetQueryExecutionOutput.queryExecution = None } -> None
  ;;

  let as_execution_id : [< t ] -> [ `Athena_execution_id of string ] option =
   fun t -> execution_id t |> Option.map ~f:(fun id -> `Athena_execution_id id)
 ;;

  let status : [< t ] -> Values.QueryExecutionStatus.t option = function
    | `Athena_execution
        { Values.GetQueryExecutionOutput.queryExecution =
            Some { Values.QueryExecution.status; _ }
        } -> status
    | `Athena_start _ | `Athena_execution_id _ -> None
    | `Athena_execution { Values.GetQueryExecutionOutput.queryExecution = None } -> None
  ;;

  let state t =
    status t
    |> Option.bind ~f:(function { Values.QueryExecutionStatus.state; _ } -> state)
  ;;

  let is_state t s =
    Option.fold ~init:false (state t) ~f:(fun acc state -> acc || Poly.( = ) state s)
  ;;

  let succeeded (t : [< t ]) = is_state t Values.QueryExecutionState.SUCCEEDED
  let canceled (t : [< t ]) = is_state t Values.QueryExecutionState.CANCELLED
  let running (t : [< t ]) = is_state t Values.QueryExecutionState.RUNNING

  let refresh cfg (t : [< t ])
    : [ `Ok of [< t > `Athena_execution ] | `Missing_execution_id ] Deferred.t
    =
    match execution_id t with
    | None -> return `Missing_execution_id
    | Some execution_id ->
      dispatch_exn
        ~name:"athena.get_query_execution"
        ~sexp_of_error:Values.GetQueryExecutionOutput.sexp_of_error
        ~f:(fun () ->
        Io.get_query_execution
          (athena_call ~cfg)
          (Values.GetQueryExecutionInput.make ~queryExecutionId:execution_id ()))
      >>| fun x -> `Ok (`Athena_execution x)
  ;;

  let get_query_results_page ?next_token ~execution_id cfg =
    Log.Global.debug
      ?tags:None
      ?time:None
      "%s %s"
      (Option.value ~default:"none" next_token)
      execution_id;
    dispatch_exn
      ~name:"athena.get_query_results"
      ~sexp_of_error:Values.GetQueryResultsOutput.sexp_of_error
      ~f:(fun () ->
      Io.get_query_results
        (athena_call ~cfg)
        (Values.GetQueryResultsInput.make
           ?nextToken:next_token
           ~queryExecutionId:execution_id
           ()))
    >>| fun x -> `Ok (`Athena_result x)
  ;;

  let results ?(close_on_exception = false) (cfg : Awsm.Cfg.t) (t : [< t ]) =
    match execution_id t with
    | None -> return @@ `Missing_execution_id
    | Some execution_id -> (
      get_query_results_page cfg ?next_token:None ~execution_id
      >>= (function
            | `Ok
                (`Athena_result
                  { Values.GetQueryResultsOutput.resultSet
                  ; nextToken = next_token
                  ; updateCount = _
                  }) -> (
              match resultSet with
              | None
              | Some { Values.ResultSet.rows = _; resultSetMetadata = None }
              | Some
                  { Values.ResultSet.rows = _
                  ; resultSetMetadata =
                      Some { Values.ResultSetMetadata.columnInfo = None }
                  } ->
                return (`Missing_result_set_metadata { execution_id; next_token = None })
              | Some
                  { Values.ResultSet.rows
                  ; resultSetMetadata =
                      Some { Values.ResultSetMetadata.columnInfo = Some column_infos }
                  } ->
                let rows = Option.value ~default:[] rows in
                return (`Ok (rows, next_token, column_infos))))
      >>= function
      | `Missing_result_set_metadata _ as e -> return e
      | `Ok (rows, next_token, result_set_metadata) ->
        let pipe =
          Pipe.create_reader ~close_on_exception (fun writer ->
            let rec folder next_token =
              match next_token with
              | None ->
                Log.Global.debug
                  ?tags:None
                  ?time:None
                  "Query.results.create_reader: %s %s"
                  (Option.value ~default:"none" next_token)
                  execution_id;
                return ()
              | Some (next_token : string) -> (
                get_query_results_page cfg ~next_token ~execution_id
                >>= function
                | `Ok
                    (`Athena_result
                      { Values.GetQueryResultsOutput.resultSet
                      ; nextToken = next_token
                      ; updateCount = _
                      }) -> (
                  match resultSet with
                  | None -> return ()
                  | Some { Values.ResultSet.rows = rows_opt; resultSetMetadata = _ } ->
                    let rows = Option.value ~default:[] rows_opt in
                    Deferred.List.iter ?how:None rows ~f:(Pipe.write writer)
                    >>= fun () -> folder next_token))
            in
            Deferred.List.iter ?how:None rows ~f:(Pipe.write writer)
            >>= fun () -> folder next_token)
        in
        return (`Ok (result_set_metadata, pipe)))
  ;;

  let output_location (t : [< t ]) =
    match t with
    | `Athena_execution_id _ -> None
    | `Athena_execution { Values.GetQueryExecutionOutput.queryExecution = None } -> None
    | `Athena_execution
        { Values.GetQueryExecutionOutput.queryExecution =
            Some
              { queryExecutionId = _
              ; query = _
              ; resultConfiguration = None
              ; queryExecutionContext = _
              ; status = _
              ; statistics = _
              ; statementType = _
              ; workGroup = _
              ; engineVersion = _
              }
        } -> None
    | `Athena_execution
        { Values.GetQueryExecutionOutput.queryExecution =
            Some
              { queryExecutionId = _
              ; query = _
              ; resultConfiguration =
                  Some
                    { outputLocation
                    ; encryptionConfiguration = _
                    ; expectedBucketOwner = _
                    ; aclConfiguration = _
                    }
              ; queryExecutionContext = _
              ; status = _
              ; statistics = _
              ; statementType = _
              ; workGroup = _
              ; engineVersion = _
              }
        } -> outputLocation
    | `Athena_start
        { query_execution_output = _
        ; result_configuration =
            { outputLocation
            ; encryptionConfiguration = _
            ; expectedBucketOwner = _
            ; aclConfiguration = _
            }
        } -> outputLocation
  ;;

  let ls ?max_results ?(close_on_exception = true) cfg
    : [< t > `Athena_execution_id ] Pipe.Reader.t
    =
    let rec paginator ?next_token writer =
      dispatch_exn
        ~name:"athena.list_query_executions"
        ~sexp_of_error:Values.ListQueryExecutionsOutput.sexp_of_error
        ~f:(fun () ->
        Io.list_query_executions
          (athena_call ~cfg)
          (Values.ListQueryExecutionsInput.make
             ?maxResults:(Option.map max_results ~f:Values.MaxQueryExecutionsCount.make)
             ?nextToken:next_token
             ()))
      >>= fun { Values.ListQueryExecutionsOutput.nextToken = next_token
              ; queryExecutionIds
              } ->
      let ids =
        Option.value ~default:[] queryExecutionIds
        |> List.map ~f:(fun s -> `Athena_execution_id s)
      in
      Deferred.List.iter ids ~f:(Pipe.write writer)
      >>= fun () ->
      match next_token with
      | None -> return ()
      | Some next_token -> paginator ~next_token writer
    in
    let f : [< t > `Athena_execution_id ] Pipe.Writer.t -> unit Deferred.t =
      paginator ?next_token:None
    in
    Pipe.create_reader ~close_on_exception f
  ;;
end

let ls_main () =
  let%bind cfg = Awsm_async.Cfg.get_exn () in
  let pipe = Query.ls ~max_results:10 cfg in
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
