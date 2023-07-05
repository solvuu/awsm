type already_exists_error = [ `AlreadyExistsException ] [@@deriving sexp]

let create_database ?catalog_id ?description ~name cfg =
  Io.create_database
    (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
    (Values.CreateDatabaseRequest.make
       ?catalogId:(Option.map ~f:Values.CatalogIdString.make catalog_id)
       ~databaseInput:
         (Values.DatabaseInput.make
            ?description:(Option.map ~f:Values.DescriptionString.make description)
            ~name:(Values.NameString.make name)
            ())
       ())
  >>= function
  | Ok x -> return x
  | Error _ -> failwithf "Glue.create_database" ()
;;
