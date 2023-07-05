open! Import

module Io = struct
  include Awsm.Http.Monad.Make (struct
    type +'a t = 'a
  end)

  let monad = { Awsm.Http.Monad.bind = (fun x f -> f (prj x)); return = (fun x -> inj x) }
  let make_stream stream () = inj (stream ())
  let make_http http meth request uri = inj (http meth request uri)
end
