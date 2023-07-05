open Lwt.Infix

let file_contents file =
  let stream = Lwt_io.chars_of_file file in
  Lwt_stream.to_string stream >>= fun contents -> Lwt.return contents
;;

module Cohttp : sig
  module Body = Cohttp_lwt.Body
  module Code = Cohttp.Code
  module Client = Cohttp_lwt_unix.Client
  module Header = Cohttp.Header
  module Request = Cohttp.Request
  module Response = Cohttp_lwt.Response

  val to_meth : Awsm.Http.Meth.t -> Cohttp.Code.meth
  val to_headers : Awsm.Http.Request.t -> Cohttp.Header.t
  val of_version : Cohttp.Response.t -> int * int
  val of_headers : Cohttp.Response.t -> Awsm.Http.Headers.t
  val of_status : Cohttp.Response.t -> Awsm.Http.Status.t
end = struct
  module Body = Cohttp_lwt.Body
  module Code = Cohttp.Code
  module Client = Cohttp_lwt_unix.Client
  module Header = Cohttp.Header
  module Request = Cohttp.Request
  module Response = Cohttp_lwt.Response

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
