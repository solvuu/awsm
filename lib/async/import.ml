(** [with_retries ?retry_delay ?retry_cnt f] wraps deferred function [f] with
    retry logic. If an exception is raise by [f], and the number of retries has
    not exceeded [retry_cnt], the exception will be logged and [f] will be
    invoked again after a span of [retry_delay] has elapsed.

    If the function [f] fails to evaluate after [retry_cnt] iterations, the
    final exception is reraised as is and nothing is logged. *)
let with_retries
  ?(retry_delay : Time.Span.t = Time.Span.of_ms 50.)
  ?(retry_cnt : int = 3)
  (f : unit -> 'a Deferred.t)
  : 'a Deferred.t
  =
  let rec retry n =
    try_with f
    >>= function
    | Result.Error e -> (
      match n >= retry_cnt with
      | true -> raise e
      | false ->
        Log.Global.debug "aws service operation failed (attempt %d/%d)" n retry_cnt;
        Clock.after retry_delay >>= fun () -> retry (n + 1))
    | Result.Ok ok -> return ok
  in
  retry 0
;;

module Cohttp : sig
  module Body = Cohttp_async.Body
  module Code = Cohttp.Code
  module Client = Cohttp_async.Client
  module Header = Cohttp.Header
  module Request = Cohttp.Request
  module Response = Cohttp_async.Response

  val to_meth : Awsm.Http.Meth.t -> Cohttp.Code.meth
  val to_headers : Awsm.Http.Request.t -> Cohttp.Header.t
  val of_version : Cohttp.Response.t -> int * int
  val of_headers : Cohttp.Response.t -> Awsm.Http.Headers.t
  val of_status : Cohttp.Response.t -> Awsm.Http.Status.t
end = struct
  module Body = Cohttp_async.Body
  module Code = Cohttp.Code
  module Client = Cohttp_async.Client
  module Header = Cohttp.Header
  module Request = Cohttp.Request
  module Response = Cohttp_async.Response

  let to_meth meth = (meth :> Cohttp.Code.meth)

  let to_headers req =
    req |> Awsm.Http.Request.headers |> Awsm.Http.Headers.to_list |> Cohttp.Header.of_list
  ;;

  let of_version resp =
    match Cohttp.Response.version resp with
    | `HTTP_1_0 -> 1, 0
    | `HTTP_1_1 -> 1, 1
    | `Other _ ->
      (* FIXME (dinosaure): should never occur! *)
      0, 0
  ;;

  let of_headers resp =
    resp |> Cohttp.Response.headers |> Cohttp.Header.to_list |> Awsm.Http.Headers.of_list
  ;;

  let of_status resp =
    match Cohttp.Response.status resp with
    | #Awsm.Http.Status.t as status -> (status :> Awsm.Http.Status.t)
    | status -> `Code (Cohttp.Code.code_of_status status)
  ;;
end

module Cohttp_async = struct end
