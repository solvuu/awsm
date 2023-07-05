open! Import

module Io : sig
  include Awsm.Http.Io.S with type 'a s := 'a and type 'a stream := unit -> 'a option
end
