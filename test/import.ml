module Cohttp = struct
  module Body = Cohttp.Body
  module Header = Cohttp.Header

  module Request = struct
    include Cohttp.Request.Make (Cohttp__String_io.M)

    let of_string x : [ `Eof | `Invalid of string | `Ok of t ] =
      Cohttp__String_io.open_in x |> read
    ;;

    let meth = Cohttp.Request.meth
    let uri = Cohttp.Request.uri
    let headers = Cohttp.Request.headers
  end
end

module Test = Awsm.Import.Test
