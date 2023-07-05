open! Import

module Io : sig
  include
    Awsm.Http.Io.S with type 'a s := 'a Deferred.t and type 'a stream := 'a Pipe.Reader.t

  val call
    :  ?endpoint_url:string
    -> cfg:Awsm.Cfg.t
    -> service:Awsm.Service.t
    -> Awsm.Http.Meth.t
    -> Awsm.Http.Request.t
    -> Uri.t
    -> ((t Awsm.Http.Response.t, Awsm.Http.Io.Error.call) result, t) Awsm.Http.Monad.app
end

include
  Awsm.Http.S
    with module Deferred := Deferred
    with module Pipe := Pipe
    with type Response.t = Cohttp.Response.t
    with type Body.t = Cohttp.Body.t
