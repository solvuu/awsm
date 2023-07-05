open! Import

module Io : sig
  include Awsm.Http.Io.S with type 'a s := 'a Lwt.t and type 'a stream := 'a Lwt_stream.t

  val call
    :  ?endpoint_url:string
    -> cfg:Awsm.Cfg.t
    -> service:Awsm.Service.t
    -> Awsm.Http.Meth.t
    -> Awsm.Http.Request.t
    -> Uri.t
    -> ((t Awsm.Http.Response.t, Awsm.Http.Io.Error.call) result, t) Awsm.Http.Monad.app
end
