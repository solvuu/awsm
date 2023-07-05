(** AWS Glue API, cohttp-async implementation *)

(** Convenience definition for already_exists_error which is often treated
    differently than other types of errors with aws glue *)
type already_exists_error = [ `AlreadyExistsException ] [@@deriving sexp]

(** [create_database ?catalog_id ?description ~name cfg] creates a glue database
    given [name] and optional [catalog_id] and [description] parameters along
    with an aws [cfg].

    Upon success, returns [`Ok unit] and one of [createDatabase_error]
    otherwise. *)
val create_database
  :  ?catalog_id:string
  -> ?description:string
  -> name:string
  -> Awsm.Cfg.t
  -> unit Deferred.t
