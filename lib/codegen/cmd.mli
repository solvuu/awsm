(** Code generation from boto service specification

    The code generated from a service comes in three parts, similar to the
    structure of service specifications: <code> (* Service metadata *) val
    service : Service.t val apiVersion : string [...]

    (* Input / Output shapes *) module FooRequest : sig ... end module
    FooResponse : sig ... end [...]

    (* service call implementations *) module Make(Http : Http.S) : sig val foo
    : Cfg.t -> FooRequest.t -> FooResponse.t Deferred.t [...] end </code>

    Each input/output type has its own module, of the form:

    <code>

    module FooRequest : sig

    type t

    val make : ... -> unit -> t

    val of_xml : ('a Xmlm.frag as 'a) Xmlm.frag -> t
    val of_json : Awsm.Json.t -> t

    val to_json : t -> Awsm.Json.t

    end

    </code> *)

open! Core
open! Import

val main : Command.t
