let io_call = Awsm_async.Http.Io.call ~service:Values.service

module Source = struct
  let default_chunk_size = Byte_units.of_megabytes 8.

  module File = struct
    let slice ~total ~file_size ~chunk_size (i : int64) =
      let open Int64 in
      let i = i - ((total - file_size) / chunk_size) in
      i * chunk_size, min (chunk_size * (i + one)) file_size - one
    ;;

    let read_slice ~start ~end_ fn =
      let open Int64 in
      let len = end_ - start + one |> Int64.to_int_exn in
      let buf = Bytes.create len in
      In_channel.with_file fn ~f:(fun ic ->
        In_channel.seek ic start;
        match In_channel.really_input ic ~buf ~pos:0 ~len with
        | None -> assert false
        | Some () -> ());
      buf |> Bytes.to_string
    ;;

    let min_part_size = 0xa00000L
    let default_num_parts = 10000L

    type stat =
      { chunk_size : int64
      ; file_size : int64
      ; partitions : int64
      }
    [@@deriving sexp]

    let params_of_file_size ~chunk_size ~file_size =
      let open Int64 in
      let chunk_size = Byte_units.bytes_int64 chunk_size in
      let partitions =
        match file_size <= min_part_size with
        | false -> min ((file_size / chunk_size) + one) default_num_parts
        | true -> one
      in
      let chunk_size =
        Int64.of_int
          (int_of_float
             (Float.round ~dir:`Up (to_float file_size /. to_float partitions)))
      in
      let chunk_size = max one chunk_size in
      { chunk_size; file_size; partitions }
    ;;

    let%expect_test "params_of_file_size" =
      let test ?(chunk_size = default_chunk_size) file_size =
        let r = params_of_file_size ~chunk_size ~file_size in
        printf !"%{sexp:stat}\n" r
      in
      test 1024L;
      [%expect {| ((chunk_size 1024) (file_size 1024) (partitions 1)) |}];
      test 2048L;
      [%expect {| ((chunk_size 2048) (file_size 2048) (partitions 1)) |}];
      test Int64.(3L * min_part_size / 2L);
      [%expect {| ((chunk_size 7864320) (file_size 15728640) (partitions 2)) |}];
      test ~chunk_size:(Byte_units.of_bytes_int 1) Int64.(2L * min_part_size);
      [%expect {| ((chunk_size 2098) (file_size 20971520) (partitions 10000)) |}];
      test 0L;
      [%expect {| ((chunk_size 1) (file_size 0) (partitions 1)) |}];
      test ~chunk_size:(Byte_units.of_bytes_int 0) 0L;
      [%expect {| ((chunk_size 1) (file_size 0) (partitions 1)) |}];
      return ()
    ;;

    let stat ?(chunk_size = default_chunk_size) file =
      Unix.stat file
      >>| fun { Unix.Stats.size = file_size; _ } ->
      params_of_file_size ~chunk_size ~file_size
    ;;

    let load_all t =
      let filename = Uri.path (Uri.of_string t) in
      let data = In_channel.with_file filename ~f:In_channel.input_all in
      let total = String.length data |> Int64.of_int in
      data, total
    ;;
  end
end

let put_object cfg ~bucket ~key body =
  let key = Values.ObjectKey.make key in
  let body = Values.Body.of_string body in
  let request = Values.PutObjectRequest.make ~body ~bucket ~key () in
  Io.put_object (io_call ~cfg) request
  >>| function
  | Error e -> Error (`Put_object e)
  | Ok response -> (
    match response.eTag with
    | Some etag -> Ok etag
    | None -> Error `Missing_etag)
;;

let delete_object cfg ~bucket ~key =
  Io.delete_object
    (io_call ~cfg)
    (Values.DeleteObjectRequest.make ~bucket ~key:(Values.ObjectKey.make key) ())
;;

let put_file cfg ~bucket ~key file =
  Reader.with_file file ~f:(fun reader ->
    let%bind body = Reader.contents reader in
    put_object cfg ~bucket ~key body)
;;

let get_object cfg ?(range : Awsm.Http.Range.t option) ~bucket ~key () =
  let key = Values.ObjectKey.make key in
  let range = Option.map ~f:Awsm.Http.Range.to_header_value range in
  let request = Values.GetObjectRequest.make ?range ~bucket ~key () in
  Io.get_object (io_call ~cfg) request
;;

type ('acc, 'error) callback =
  'acc
  -> total:int64
  -> loaded:int64
  -> key:string
  -> part:int64
  -> num_parts:int64
  -> [ `Complete of Values.ETag.t
     | `Initial of Values.MultipartUploadId.t
     | `Partition of Values.ETag.t
     ]
  -> ('acc, 'error) Deferred.Result.t

module Error = struct
  type ('acc, 'error) multipart =
    [ `Missing_upload_id
    | `Callback_error of 'acc * Values.CompletedPartList.t * 'error
    ]
  [@@deriving sexp]
end

let initialize_multipart cfg ~bucket ~key =
  let key_obj = Values.ObjectKey.make key in
  let req = Values.CreateMultipartUploadRequest.make ~bucket ~key:key_obj () in
  Awsm_async.Import.with_retries
  @@ fun () ->
  Io.create_multipart_upload (io_call ~cfg) req
  >>| function
  | Ok { Values.CreateMultipartUploadOutput.uploadId = Some uploadId; _ } ->
    Ok (`Upload_id uploadId)
  | Ok { Values.CreateMultipartUploadOutput.uploadId = None; _ } ->
    Error `Missing_upload_id
  | Error e -> Error (`Create_multipart_upload e)
;;

let multipart
  cfg
  ?chunk_size
  ?(part = 0)
  ?(file_offset = Int64.zero)
  ~bucket
  ~key
  ~init
  ~(cb : ('acc, 'error) callback)
  ~upload_id
  file
  =
  Source.File.stat ?chunk_size file
  >>= fun { chunk_size; file_size; partitions = rem_parts } ->
  let key_obj = Values.ObjectKey.make key in
  let total = Int64.(file_size + file_offset) in
  let loaded = Int64.(of_int part * chunk_size) in
  let part = Int64.of_int part in
  let num_parts = Int64.((file_offset / chunk_size) + rem_parts) in
  cb init ~total ~loaded ~key ~part ~num_parts (`Initial upload_id)
  >>| (function
        | Result.Ok acc -> `Ok (acc, upload_id)
        | Result.Error e -> `Callback_error (init, [], e))
  >>= function
  | `Callback_error _ as e -> return (Error e)
  | `Ok (acc, upload_id) -> (
    let upload_part acc (i : int64) =
      let open Int64 in
      let start, end_ = Source.File.slice ~total ~file_size ~chunk_size i in
      let part = Source.File.read_slice ~start ~end_ file in
      let i' = i + one in
      let contentLength = String.length part |> Int64.of_int in
      let contentMD5 = Awsm.Client.content_md5 part in
      let upload_part_request =
        Values.UploadPartRequest.make
          ~bucket
          ~uploadId:upload_id
          ~partNumber:(to_int_exn i')
          ~body:(Values.Body.of_string part)
          ~contentLength
          ~key:key_obj
          ~contentMD5
          ()
      in
      Io.upload_part (io_call ~cfg) upload_part_request
      >>= function
      | Error e -> return (Error (`Upload_part e))
      | Ok uploadPartResp -> (
        let eTag = Option.value_exn uploadPartResp.Values.UploadPartOutput.eTag in
        cb
          acc
          ~total
          ~loaded:(Int64.succ end_ + file_offset)
          ~key
          ~part:(i + one)
          ~num_parts
          (`Partition eTag)
        >>= function
        | Result.Error e -> return (Error (`Callback_error e))
        | Result.Ok acc ->
          return
          @@ Ok (acc, Values.CompletedPart.make ~eTag ~partNumber:(to_int_exn i') ()))
    in
    num_parts
    |> Int64.to_int_exn
    |> List.range (part |> Int64.to_int_exn)
    |> List.map ~f:Int64.of_int
    |> Deferred.List.fold
         ~init:(Ok (acc, []))
         ~f:(fun acc_etags part ->
           match acc_etags with
           | Error _ as e -> return e
           | Ok (acc, etags) -> (
             upload_part acc part
             >>= function
             | Ok (acc', etag) -> return @@ Ok (acc', etag :: etags)
             | Error (`Callback_error e) ->
               return @@ Error (`Callback_error (acc, etags, e))
             | Error (`Upload_part e) -> return @@ Error (`Upload_part e)))
    >>= function
    | Error e -> return (Error e)
    | Ok (acc, rev_etags) -> (
      let multipartUpload =
        Values.CompletedMultipartUpload.make ~parts:(List.rev rev_etags) ()
      in
      let req =
        Values.CompleteMultipartUploadRequest.make
          ~multipartUpload
          ~bucket
          ~key:key_obj
          ~uploadId:upload_id
          ()
      in
      Io.complete_multipart_upload (io_call ~cfg) req
      >>= function
      | Error e -> return (Error (`Complete_multipart_upload e))
      | Ok completedUpload -> (
        cb
          acc
          ~total
          ~loaded:total
          ~key
          ~part:num_parts
          ~num_parts
          (`Complete (Option.value_exn completedUpload.eTag))
        >>| function
        | Result.Ok acc' -> Ok (acc', List.rev rev_etags)
        | Result.Error e -> Error (`Callback_error (acc, List.rev rev_etags, e)))))
;;

let map_bucket cfg ~bucket ~f =
  let rec loop ?nextContinuationToken () =
    match%bind
      Io.list_objects_v2
        (io_call ~cfg)
        (Values.ListObjectsV2Request.make
           ?delimiter:None
           ?encodingType:None
           ?maxKeys:None
           ?prefix:None
           ?continuationToken:nextContinuationToken
           ?fetchOwner:None
           ?startAfter:None
           ?requestPayer:None
           ~bucket
           ())
    with
    | Error a -> return (Error a)
    | Ok
        { isTruncated
        ; contents
        ; name = _
        ; prefix = _
        ; delimiter = _
        ; maxKeys = _
        ; commonPrefixes = _
        ; encodingType = _
        ; keyCount = _
        ; continuationToken = _
        ; nextContinuationToken
        ; startAfter = _
        } -> (
      match contents with
      | None -> return (Ok [])
      | Some contents -> (
        let%bind result = Deferred.List.map ~how:`Sequential contents ~f in
        match Option.value isTruncated ~default:false with
        | false -> return (Ok result)
        | true -> (
          let%map rest = loop ?nextContinuationToken () in
          match rest with
          | Ok rest -> Ok (result @ rest)
          | Error e -> Error e)))
  in
  loop ()
;;

let iter_bucket cfg ~bucket ~f =
  match%map map_bucket cfg ~bucket ~f with
  | Ok (_ : unit list) -> Ok ()
  | Error e -> Error e
;;
