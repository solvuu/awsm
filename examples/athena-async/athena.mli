(** AWS Athena Async API *)

module Values = Awsm_athena_async.Values
module Io = Awsm_athena_async.Io

module Query : sig
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

  val of_id : string -> [< t > `Athena_execution_id ]

  val submit
    :  ?idem_potency_token:string
    -> ?output_location:string
    -> query_string:string
    -> bucket:string
    -> Awsm.Cfg.t
    -> [ `Ok of [< t > `Athena_start ] ] Deferred.t

  val execution_id : [< t ] -> string option
  val output_location : [< t ] -> string option
  val status : [< t ] -> Values.QueryExecutionStatus.t option
  val state : [< t ] -> Values.QueryExecutionState.t option
  val is_state : [< t ] -> Values.QueryExecutionState.t -> bool
  val succeeded : [< t ] -> bool
  val canceled : [< t ] -> bool
  val running : [< t ] -> bool

  val refresh
    :  Awsm.Cfg.t
    -> [< t ]
    -> [ `Missing_execution_id | `Ok of [< t > `Athena_execution ] ] Deferred.t

  val results
    :  ?close_on_exception:bool
    -> Awsm.Cfg.t
    -> [< t ]
    -> [ `Missing_execution_id
       | `Missing_result_set_metadata of query_id_params
       | `Ok of Values.ColumnInfo.t list * Values.Row.t Pipe.Reader.t
       ]
       Deferred.t

  val ls
    :  ?max_results:int
    -> ?close_on_exception:bool
    -> Awsm.Cfg.t
    -> [< t > `Athena_execution_id ] Pipe.Reader.t

  val as_execution_id : [< t ] -> [ `Athena_execution_id of string ] option
end
