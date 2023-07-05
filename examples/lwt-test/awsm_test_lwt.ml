open Lwt.Infix
module Cfg = Awsm_lwt.Cfg

module Ec2 = struct
  include Awsm_ec2_lwt.Import
  module Values = Awsm_ec2_lwt.Values
  module Io = Awsm_ec2_lwt.Io

  let call = Awsm.Http.Io.call ~service:Values.service
end

module Ecs = struct
  include Awsm_ecs_lwt.Import
  module Values = Awsm_ecs_lwt.Values
  module Io = Awsm_ecs_lwt.Io

  let call = Awsm.Http.Io.call ~service:Values.service
end

module Ecr = struct
  include Awsm_ecr_lwt.Import
  module Values = Awsm_ecr_lwt.Values
  module Io = Awsm_ecr_lwt.Io

  let call = Awsm.Http.Io.call ~service:Values.service
end

module S3 = struct
  include Awsm_s3_lwt.Import
  module Values = Awsm_s3_lwt.Values
  module Io = Awsm_s3_lwt.Io

  let call = Awsm.Http.Io.call ~service:Values.service
end

let pr = Caml.print_endline

let dispatch_exn ~name ~sexp_of_error ~f =
  match%bind f () with
  | Ok v -> return v
  | Error (`Transport err) ->
    failwithf "%s: %s" name (Awsm.Http.Io.Error.sexp_of_call err |> Sexp.to_string_hum) ()
  | Error (`AWS aws) ->
    failwithf "%s: %s" name (aws |> sexp_of_error |> Sexp.to_string_hum) ()
;;

let suite_main ~sso bucket () =
  let%bind cfg =
    match sso with
    | true -> Awsm_sso_lwt.Util.Cfg.get_exn ()
    | false -> Cfg.get_exn ()
  in
  let%bind () =
    pr "=== EC2 ===";
    dispatch_exn
      ~name:"ec2.describe_instances"
      ~sexp_of_error:Ec2.Values.Ec2_error.sexp_of_t
      ~f:(fun () ->
      Ec2.Io.describe_instances
        (Ec2.call ~cfg)
        (Ec2.Values.DescribeInstancesRequest.make ()))
    >|= fun v ->
    Option.iter v.Ec2.Values.DescribeInstancesResult.reservations ~f:(fun reservation ->
      List.iter reservation ~f:(fun x ->
        Option.iter x.Ec2.Values.Reservation.ownerId ~f:pr))
  in
  let%bind () =
    pr "=== ECS ===";
    dispatch_exn
      ~name:"ecs.describe_clusters"
      ~sexp_of_error:Ecs.Values.DescribeClustersResponse.sexp_of_error
      ~f:(fun () ->
      Ecs.Io.describe_clusters
        (Ecs.call ~cfg)
        (Ecs.Values.DescribeClustersRequest.make ()))
    >|= fun v ->
    Option.iter v.Ecs.Values.DescribeClustersResponse.clusters ~f:(fun cluster ->
      List.iter cluster ~f:(fun repo ->
        Option.iter repo.Ecs.Values.Cluster.clusterName ~f:pr))
  in
  let%bind () =
    pr "=== ECR ===";
    let ecr_call = Ecr.call ~cfg in
    let repositoryName = Ecr.Values.RepositoryName.make "delme/delme" in
    let%bind () =
      dispatch_exn
        ~name:"ecr.get_authorization_token"
        ~sexp_of_error:Ecr.Values.GetAuthorizationTokenResponse.sexp_of_error
        ~f:(fun () ->
        Ecr.Io.get_authorization_token
          ecr_call
          (Ecr.Values.GetAuthorizationTokenRequest.make ()))
      >|= fun v ->
      Option.iter
        v.Ecr.Values.GetAuthorizationTokenResponse.authorizationData
        ~f:(fun xl ->
        List.iter xl ~f:(fun ad ->
          Option.iter
            (ad.Ecr.Values.AuthorizationData.authorizationToken :> string option)
            ~f:pr))
    in
    let%bind () =
      dispatch_exn
        ~name:"ecr.create_repository"
        ~sexp_of_error:Ecr.Values.CreateRepositoryResponse.sexp_of_error
        ~f:(fun () ->
        Ecr.Io.create_repository
          ecr_call
          (Ecr.Values.CreateRepositoryRequest.make ~repositoryName ()))
      >|= fun _v -> ()
    in
    let%bind () =
      dispatch_exn
        ~name:"ecr.describe_repositories"
        ~sexp_of_error:Ecr.Values.DescribeRepositoriesResponse.sexp_of_error
        ~f:(fun () ->
        Ecr.Io.describe_repositories
          ecr_call
          (Ecr.Values.DescribeRepositoriesRequest.make ()))
      >>= fun v ->
      Option.value_map
        v.Ecr.Values.DescribeRepositoriesResponse.repositories
        ~default:(return ())
        ~f:(fun repos ->
        let foreach repo =
          Option.value_map
            repo.Ecr.Values.Repository.repositoryName
            ~default:(return (Ok ()))
            ~f:(fun repositoryName ->
            pr (repositoryName :> string);
            dispatch_exn
              ~name:"ecr.list_images"
              ~sexp_of_error:Ecr.Values.ListImagesResponse.sexp_of_error
              ~f:(fun () ->
              Ecr.Io.list_images
                ecr_call
                (Ecr.Values.ListImagesRequest.make ~repositoryName ()))
            >|= fun images ->
            let imageIds =
              Option.value
                (images.Ecr.Values.ListImagesResponse.imageIds
                  :> Ecr.Values.ImageIdentifier.t list option)
                ~default:[]
            in
            Ok
              (List.iter imageIds ~f:(fun id ->
                 Option.iter id.Ecr.Values.ImageIdentifier.imageTag ~f:(fun id ->
                   pr ("\t" ^ id)))))
        in
        Lwt_list.map_s foreach repos >|= Result.all >|= ignore)
    in
    let%bind () =
      dispatch_exn
        ~name:"ecr.delete_repository"
        ~sexp_of_error:Ecr.Values.DeleteRepositoryResponse.sexp_of_error
        ~f:(fun () ->
        Ecr.Io.delete_repository
          ecr_call
          (Ecr.Values.DeleteRepositoryRequest.make ~repositoryName ()))
      >|= fun _v -> ()
    in
    return ()
  in
  let%bind () =
    pr "=== S3 ===";
    let s3_call = S3.call ~cfg in
    let%bind () =
      dispatch_exn
        ~name:"s3.list_buckets"
        ~sexp_of_error:S3.Values.ListBucketsOutput.sexp_of_error
        ~f:(fun () -> S3.Io.list_buckets s3_call ())
      >|= fun _ -> ()
    in
    dispatch_exn
      ~name:"s3.list_objects"
      ~sexp_of_error:S3.Values.ListObjectsOutput.sexp_of_error
      ~f:(fun () ->
      S3.Io.list_objects s3_call (S3.Values.ListObjectsRequest.make ~bucket ()))
    >|= fun v ->
    Option.iter v.S3.Values.ListObjectsOutput.name ~f:pr;
    let contents = Option.value ~default:[] v.S3.Values.ListObjectsOutput.contents in
    List.iter contents ~f:(fun oo ->
      Option.iter (oo.S3.Values.Object.key :> string option) ~f:pr)
  in
  return ()
;;

let default_chunk_size = 10 * 1024 * 1024

let slice ~file_size ~chunk_size i =
  i * chunk_size, min (chunk_size * (i + 1)) file_size - 1
;;

let read_slice ~start ~end_ fn =
  Lwt_preemptive.detach
    (fun () ->
      let len = end_ - start + 1 in
      let buf = Bytes.create len in
      In_channel.with_file fn ~f:(fun ic ->
        In_channel.seek ic (Int64.of_int start);
        match In_channel.really_input ic ~buf ~pos:0 ~len with
        | None -> assert false
        | Some () -> ());
      buf |> Bytes.to_string)
    ()
;;

let multipart_main ~sso bucket key file () =
  let%bind cfg =
    match sso with
    | true -> Awsm_sso_lwt.Util.Cfg.get_exn ()
    | false -> Cfg.get_exn ()
  in
  let s3_call = S3.call ~cfg in
  Lwt_unix.stat file
  >>= fun { st_size = file_size; _ } ->
  let nb_parts = min ((file_size / default_chunk_size) + 1) 10000 in
  let chunk_size =
    int_of_float (Float.round ~dir:`Up (float file_size /. float nb_parts))
  in
  let%bind uploadId =
    dispatch_exn
      ~name:"s3.create_multipart"
      ~sexp_of_error:S3.Values.CreateMultipartUploadOutput.sexp_of_error
      ~f:(fun () ->
      S3.Io.create_multipart_upload
        s3_call
        (S3.Values.CreateMultipartUploadRequest.make
           ~bucket
           ~key:(S3.Values.ObjectKey.make key)
           ()))
    >|= function
    | { S3.Values.CreateMultipartUploadOutput.uploadId; _ } ->
      Option.value_exn ~message:"no uploadId" uploadId
  in
  let upload_part i =
    let start, end_ = slice ~file_size ~chunk_size i in
    let%bind part = read_slice ~start ~end_ file in
    dispatch_exn
      ~name:"s3.upload_part_request"
      ~sexp_of_error:S3.Values.UploadPartOutput.sexp_of_error
      ~f:(fun () ->
      S3.Io.upload_part
        s3_call
        (S3.Values.UploadPartRequest.make
           ~bucket
           ~uploadId
           ~partNumber:(i + 1)
           ~body:(S3.Values.Body.of_string part)
           ~contentLength:(part |> String.length |> Int64.of_int)
           ~key
           ~contentMD5:(Awsm.Client.content_md5 part)
           ()))
    >|= fun uploadPartResp ->
    let eTag = Option.value_exn uploadPartResp.S3.Values.UploadPartOutput.eTag in
    printf "%d: eTag = %s\n%!" i eTag;
    S3.Values.CompletedPart.make ~eTag ~partNumber:(i + 1) ()
  in
  Lwt_list.map_s upload_part (List.init nb_parts ~f:Fn.id)
  >>= fun upload_threads ->
  Lwt_list.fold_left_s (fun accu x -> return (x :: accu)) [] upload_threads
  >>= fun rev_etags ->
  dispatch_exn
    ~name:"s3.completed_multipart_upload_request"
    ~sexp_of_error:S3.Values.CompleteMultipartUploadOutput.sexp_of_error
    ~f:(fun () ->
    let req =
      S3.Values.CompleteMultipartUploadRequest.make
        ~multipartUpload:
          (S3.Values.CompletedMultipartUpload.make ~parts:(List.rev rev_etags) ())
        ~bucket
        ~key
        ~uploadId
        ()
    in
    S3.Io.complete_multipart_upload s3_call req)
  >|= fun _ -> ()
;;

module Param = struct
  let bucket = Command.Param.(anon ("bucket" %: string))
  let key = Command.Param.(anon ("key" %: string))
  let file = Command.Param.(anon ("file" %: Filename_unix.arg_type))
end

let suite_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Test script"
    [%map_open
      let bucket = Param.bucket in
      fun () -> Lwt_main.run (suite_main ~sso:false bucket ())]
;;

let sso_suite_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Test script"
    [%map_open
      let bucket = Param.bucket in
      fun () -> Lwt_main.run (suite_main ~sso:true bucket ())]
;;

let multipart_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Multipart upload test"
    [%map_open
      let bucket = Param.bucket
      and key = Param.key
      and file = Param.file in
      fun () -> Lwt_main.run (multipart_main ~sso:false bucket key file ())]
;;

let sso_multipart_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Multipart upload test (but with Sso auth)"
    [%map_open
      let bucket = Param.bucket
      and key = Param.key
      and file = Param.file in
      fun () -> Lwt_main.run (multipart_main ~sso:true bucket key file ())]
;;

let () =
  Command.group
    ~summary:"Awsm test app"
    [ "test-suite", suite_command
    ; "sso-test-suite", sso_suite_command
    ; "multipart-test", multipart_command
    ; "sso-multipart-test", sso_multipart_command
    ]
  |> Command_unix.run
;;
