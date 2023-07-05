let pp_opt pp ppf = function
  | None -> Format.fprintf ppf "<not present>"
  | Some x -> pp ppf x
;;

let failwithf fmt = Format.kasprintf failwith fmt

let dispatch_exn ~name ~sexp_of_error ~f =
  match%bind f () with
  | Ok v -> return v
  | Error (`Transport err) ->
    failwithf "%s: %s" name (Awsm.Http.Io.Error.sexp_of_call err |> Sexp.to_string_hum) ()
  | Error (`AWS aws) ->
    failwithf "%s: %s" name (aws |> sexp_of_error |> Sexp.to_string_hum) ()
;;

module List_buckets = struct
  let pp_created_at ppf s = Format.fprintf ppf " (created at %s)" s

  let pp_bucket_line ppf { Values.Bucket.name; creationDate } =
    Format.fprintf
      ppf
      "- %a%a\n"
      (Fmt.option Fmt.string)
      name
      (Fmt.option pp_created_at)
      creationDate
  ;;

  let owner_opt_to_string = function
    | None -> "<no owner>"
    | Some (owner : Values.Owner.t) ->
      Option.value owner.displayName ~default:"<no displayname>"
  ;;

  let pp_out ppf { Values.ListBucketsOutput.owner; buckets } =
    let owner = owner_opt_to_string owner in
    Format.fprintf
      ppf
      "Owner: %s\nBuckets:\n%a"
      owner
      (Fmt.option (Fmt.list pp_bucket_line))
      buckets
  ;;

  let run () =
    Awsm_async.Cfg.get_exn ()
    >>= fun cfg ->
    dispatch_exn
      ~name:"list_buckets"
      ~sexp_of_error:Values.ListBucketsOutput.sexp_of_error
      ~f:(fun () ->
      Io.list_buckets (Awsm_async.Http.Io.call ~cfg ~service:Values.service) ())
    >>| fun v -> Format.printf "%a" pp_out v
  ;;

  let param =
    let open Command.Param in
    return run
  ;;

  let command = Command.async ~summary:"List buckets" param
end

module List_objects = struct
  let pp_size ppf n = Format.fprintf ppf ", %d bytes" n
  let pp_last_modified ppf s = Format.fprintf ppf ", last modified %s" s

  let pp_object ppf { Values.Object.key; size; lastModified; _ } =
    Format.fprintf
      ppf
      "- %a%a%a\n"
      (Fmt.option Fmt.string)
      key
      (Fmt.option pp_size)
      size
      (Fmt.option pp_last_modified)
      lastModified
  ;;

  let pp_out ppf { Values.ListObjectsOutput.contents; _ } =
    Format.fprintf ppf "%a" (Fmt.option (Fmt.list pp_object)) contents
  ;;

  let run bucket () =
    Awsm_async.Cfg.get_exn ()
    >>= fun cfg ->
    dispatch_exn
      ~name:"list_objects"
      ~sexp_of_error:Values.ListObjectsOutput.sexp_of_error
      ~f:(fun () ->
      Io.list_objects
        (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
        (Values.ListObjectsRequest.make ~bucket ~prefix:"" ()))
    >>| fun v -> Format.printf "%a" pp_out v
  ;;

  let param =
    let open Command.Param in
    return run <*> Awsm_async.Param.bucket
  ;;

  let command = Command.async ~summary:"List objects in a bucket" param
end

module Get_object = struct
  let save { Values.GetObjectOutput.body; _ } ~to_:dest =
    let data = Option.value_exn body in
    Out_channel.write_all dest ~data
  ;;

  let pp_metadata_kv ppf (k, v) = Format.fprintf ppf "%s => %s" k v

  let pp_metadata =
    let sep ppf () = Format.fprintf ppf ", " in
    Fmt.list ~sep pp_metadata_kv
  ;;

  let pp_metadata
    ppf
    { Values.GetObjectOutput.lastModified; contentLength; eTag; contentType; metadata; _ }
    =
    Format.fprintf
      ppf
      "Last modified: %a\nContent length: %a\nETag: %a\nContent-Type: %a\nMetadata: %a\n"
      (pp_opt String.pp)
      lastModified
      (pp_opt Int64.pp)
      contentLength
      (pp_opt String.pp)
      eTag
      (pp_opt String.pp)
      contentType
      (pp_opt pp_metadata)
      metadata
  ;;

  let run bucket key dest_opt () =
    Awsm_async.Cfg.get_exn ()
    >>= fun cfg ->
    dispatch_exn
      ~name:"get_object"
      ~sexp_of_error:Values.GetObjectOutput.sexp_of_error
      ~f:(fun () ->
      Io.get_object
        (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
        (Values.GetObjectRequest.make ~bucket ~key ()))
    >>| fun out ->
    match dest_opt with
    | None -> Format.printf "%a" pp_metadata out
    | Some dest -> save ~to_:dest out
  ;;

  let destination =
    let open Command.Param in
    flag "-o" (optional Filename_unix.arg_type) ~doc:"Save output to this file"
  ;;

  let param =
    let open Command.Param in
    return run <*> Awsm_async.Param.bucket <*> Awsm_async.Param.key <*> destination
  ;;

  let command = Command.async ~summary:"Download an object" param
end

module Put_object = struct
  let pp_out ppf { Values.PutObjectOutput.eTag; _ } =
    Format.fprintf ppf "ETag: %a\n" (pp_opt String.pp) eTag
  ;;

  let run bucket key infile () =
    let body_s = In_channel.read_all infile in
    let body = Values.Body.of_string body_s in
    Awsm_async.Cfg.get_exn ()
    >>= fun cfg ->
    dispatch_exn
      ~name:"put_object"
      ~sexp_of_error:Values.PutObjectOutput.sexp_of_error
      ~f:(fun () ->
      Io.put_object
        (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
        (Values.PutObjectRequest.make ~bucket ~key ~body ()))
    >>| fun v -> Format.printf "%a" pp_out v
  ;;

  let param =
    let open Command.Param in
    return run
    <*> Awsm_async.Param.bucket
    <*> Awsm_async.Param.key
    <*> Awsm_async.Param.infile
  ;;

  let command = Command.async ~summary:"Upload an object" param
end

module Put_multipart = struct
  let run bucket key infile () =
    Awsm_async.Cfg.get_exn ()
    >>= fun cfg ->
    dispatch_exn
      ~name:"create_multipart_upload"
      ~sexp_of_error:Values.CreateMultipartUploadOutput.sexp_of_error
      ~f:(fun () ->
      Io.create_multipart_upload
        (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
        (Values.CreateMultipartUploadRequest.make ~bucket ~key ()))
    >>= fun creation ->
    let upload_part ~nparts i part =
      let req = Part.upload_request ~creation ~path:infile part in
      let progress = 100. *. float i /. float nparts in
      printf "%d/%d (%.1f%%)\n%!" i nparts progress;
      dispatch_exn
        ~name:"upload_part"
        ~sexp_of_error:Values.UploadPartOutput.sexp_of_error
        ~f:(fun () ->
        Io.upload_part (Awsm_async.Http.Io.call ~cfg ~service:Values.service) req)
      >>| fun v -> Part.completed_part part v
    in
    let parts = Part.build_parts infile in
    let nparts = Core.List.length parts in
    Deferred.List.mapi parts ~f:(upload_part ~nparts)
    >>= fun parts ->
    dispatch_exn
      ~name:"complete_multipart_upload"
      ~sexp_of_error:Values.CompleteMultipartUploadOutput.sexp_of_error
      ~f:(fun () ->
      Io.complete_multipart_upload
        (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
        (Part.complete_request ~creation ~parts))
    >>| fun { location; _ } ->
    Format.printf
      "Successful upload, location: %a\n"
      (pp_opt Format.pp_print_string)
      location
  ;;

  let param =
    let open Command.Param in
    return run
    <*> Awsm_async.Param.bucket
    <*> Awsm_async.Param.key
    <*> Awsm_async.Param.infile
  ;;

  let command = Command.async ~summary:"Upload an object using the multipart API" param
end

let main =
  Command.group
    ~summary:"Interact with the S3 API"
    [ "lb", List_buckets.command
    ; "ls", List_objects.command
    ; "get", Get_object.command
    ; "put", Put_object.command
    ; "put-multipart", Put_multipart.command
    ]
;;
