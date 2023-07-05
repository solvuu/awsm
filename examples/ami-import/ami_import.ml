module Cfg = Awsm_async.Cfg

module Ec2 = struct
  module Values = Awsm_ec2_async.Values
  module Io = Awsm_ec2_async.Io

  let call = Awsm_async.Http.Io.call ~service:Values.service
end

module Ebs = struct
  module Values = Awsm_ebs_async.Values
  module Io = Awsm_ebs_async.Io

  let call = Awsm_async.Http.Io.call ~service:Values.service
end

(* This is a port of https://github.com/ipxe/ipxe/blob/master/contrib/cloud/aws-import *)
let block_size = 512 * 1024
let checksum_algorithm = "SHA256"

let detect_architecture image =
  let default = "x86_64" in
  match%bind
    Async.Process.run ~prog:"mdir" ~args:[ "-b"; "-i"; image; "::/EFI/BOOT" ] ()
  with
  | Error e ->
    eprintf "detect_architecture: %s\n" (Error.to_string_hum e);
    eprintf "detect_architecture: defaulting to %s\n" default;
    return default
  | Ok mdir -> (
    match String.is_substring ~substring:"BOOTAA64.EFI" mdir with
    | true -> return "arm64"
    | false -> return default)
;;

let raise_transport_error ~name err =
  failwithf "%s: %s" name (Awsm.Http.Io.Error.sexp_of_call err |> Sexp.to_string_hum) ()
;;

let dispatch_exn ~name ~sexp_of_error ~f =
  match%bind f () with
  | Ok v -> return v
  | Error (`Transport err) -> raise_transport_error ~name err
  | Error (`AWS aws) ->
    failwithf "%s: %s" name (aws |> sexp_of_error |> Sexp.to_string_hum) ()
;;

let start_snapshot ~cfg ~volume_size ~description =
  dispatch_exn
    ~name:"start_snapshot"
    ~sexp_of_error:Ebs.Values.StartSnapshotResponse.sexp_of_error
    ~f:(fun () ->
    Ebs.Io.start_snapshot
      (Ebs.call ~cfg)
      (Ebs.Values.StartSnapshotRequest.make
         ~description
         ~volumeSize:(Int64.of_int volume_size)
         ()))
  >>| fun v ->
  Option.value_exn
    ~message:"snapshotId is None"
    v.Ebs.Values.StartSnapshotResponse.snapshotId
;;

let put_snapshot_block ~cfg request =
  dispatch_exn
    ~name:"put_snapshot_block"
    ~sexp_of_error:Ebs.Values.PutSnapshotBlockResponse.sexp_of_error
    ~f:(fun () -> Ebs.Io.put_snapshot_block (Ebs.call ~cfg) request)
  >>| fun v ->
  let _checksum = v.Ebs.Values.PutSnapshotBlockResponse.checksum in
  ()
;;

let complete_snapshot ~cfg ~snapshot_id ~changed_blocks_count =
  dispatch_exn
    ~name:"complete_snapshot"
    ~sexp_of_error:Ebs.Values.CompleteSnapshotResponse.sexp_of_error
    ~f:(fun () ->
    Ebs.Io.complete_snapshot
      (Ebs.call ~cfg)
      (Ebs.Values.CompleteSnapshotRequest.make
         ~snapshotId:snapshot_id
         ~changedBlocksCount:changed_blocks_count
         ()))
  >>| fun v ->
  Option.value_exn
    ~message:"No completeSnapshotResponse.status"
    v.Ebs.Values.CompleteSnapshotResponse.status
;;

let describe_snapshots ~cfg ~snapshot_id =
  dispatch_exn
    ~name:"describe_snapshots"
    ~sexp_of_error:Ec2.Values.Ec2_error.sexp_of_t
    ~f:(fun () ->
    Ec2.Io.describe_snapshots
      (Ec2.call ~cfg)
      (Ec2.Values.DescribeSnapshotsRequest.make ~snapshotIds:[ snapshot_id ] ()))
  >>| fun v ->
  match v.Ec2.Values.DescribeSnapshotsResult.snapshots with
  | None -> failwithf "No snapshots for %s at all" snapshot_id ()
  | Some [ { state; _ } ] -> Option.value_exn ~message:"No snapshot state" state
  | Some lst ->
    failwithf
      "Snapshots list length %d <> 1 (expected %s)"
      (List.length lst)
      snapshot_id
      ()
;;

let describe_images ~cfg ~image_id =
  dispatch_exn
    ~name:"describe_images"
    ~sexp_of_error:Ec2.Values.Ec2_error.sexp_of_t
    ~f:(fun () ->
    Ec2.Io.describe_images
      (Ec2.call ~cfg)
      (Ec2.Values.DescribeImagesRequest.make ~imageIds:[ image_id ] ()))
  >>| fun v ->
  match v.Ec2.Values.DescribeImagesResult.images with
  | None -> failwithf "No images for %s at all" image_id ()
  | Some [ { state; _ } ] -> Option.value_exn ~message:"No image state" state
  | Some lst ->
    failwithf "Images list length %d <> 1 (expected '%s')" (List.length lst) image_id ()
;;

let waiter_retry_logic ~f ~max_attempts ~delay =
  let rec loop attempt =
    match Int.( >= ) attempt max_attempts with
    | true ->
      failwithf
        !"waiter_retry_logic: gave up after %d attempts (%{Time.Span} per attempt)"
        max_attempts
        delay
        ()
    | false -> (
      let%bind () =
        match attempt with
        | 0 -> return ()
        | _ -> Clock.after delay
      in
      match%bind f () with
      | `ok -> return ()
      | `retry -> loop (Int.succ attempt))
  in
  loop 0
;;

let snapshot_completed_waiter ~cfg ~snapshot_id =
  waiter_retry_logic ~delay:(sec 15.) ~max_attempts:40 ~f:(fun () ->
    match%map describe_snapshots ~cfg ~snapshot_id with
    | Ec2.Values.SnapshotState.Completed -> `ok
    | Error -> failwithf "snapshot state for %s settled to error" snapshot_id ()
    | _ -> `retry)
;;

let image_available_waiter ~cfg ~image_id =
  waiter_retry_logic ~delay:(sec 15.) ~max_attempts:40 ~f:(fun () ->
    match%map describe_images ~cfg ~image_id with
    | Ec2.Values.ImageState.Available -> `ok
    | Failed -> failwithf "iamge state for %s settled to error" image_id ()
    | _ -> `retry)
;;

let all_regions ~cfg =
  dispatch_exn
    ~name:"describe_regions"
    ~sexp_of_error:Ec2.Values.Ec2_error.sexp_of_t
    ~f:(fun () ->
    Ec2.Io.describe_regions (Ec2.call ~cfg) (Ec2.Values.DescribeRegionsRequest.make ()))
  >>| fun v ->
  Option.value_exn ~message:"regions is None" v.Ec2.Values.DescribeRegionsResult.regions
;;

let create_snapshot ~cfg ~description ~image =
  let%bind snapshot_id = start_snapshot ~cfg ~volume_size:1 ~description in
  let%bind r = Reader.open_file image in
  let buf = Bytes.create block_size in
  let rec put_block_loop block_index =
    match%bind Reader.read r ~pos:0 ~len:block_size buf with
    | `Eof -> return block_index
    | `Ok bytes_read ->
      (* The python script does this padding with zeroes, though I'm not sure this is
         always safe; this may corrupt file reads that return short, if it's not short
         because we're at EOF. *)
      for i = bytes_read to Int.pred block_size do
        Bytes.set buf i '\x00'
      done;
      let block_data = Bytes.to_string buf in
      let checksum =
        block_data
        |> Cryptokit.hash_string (Cryptokit.Hash.sha256 ())
        |> Base64.encode_exn
      in
      let%bind () =
        put_snapshot_block
          ~cfg
          (Ebs.Values.PutSnapshotBlockRequest.make
             ~snapshotId:snapshot_id
             ~blockIndex:block_index
             ~blockData:(Ebs.Values.BlockData.of_string block_data)
             ~dataLength:block_size
             ~checksum
             ~checksumAlgorithm:
               (Ebs.Values.ChecksumAlgorithm.of_string checksum_algorithm)
             ())
      in
      put_block_loop (Int.succ block_index)
  in
  let%bind last_block_index = put_block_loop 0 in
  let%bind _status =
    complete_snapshot ~cfg ~snapshot_id ~changed_blocks_count:last_block_index
  in
  return snapshot_id
;;

let register_image ~cfg request =
  dispatch_exn
    ~name:"register_image"
    ~f:(fun () -> Ec2.Io.register_image (Ec2.call ~cfg) request)
    ~sexp_of_error:Ec2.Values.Ec2_error.sexp_of_t
  >>| fun v ->
  Option.value_exn
    ~message:"No registerImageResult.imageId"
    v.Ec2.Values.RegisterImageResult.imageId
;;

let import_image ~cfg ~name ~architecture ~image =
  let region =
    Option.value_exn ~message:"AWS config does not have region set" cfg.Awsm.Cfg.region
  in
  let description = sprintf "%s (%s)" name architecture in
  let%bind snapshot_id = create_snapshot ~cfg ~description ~image in
  let%bind () = snapshot_completed_waiter ~cfg ~snapshot_id in
  let%bind image_id =
    let device_name = "/dev/sda1" in
    let ebs =
      Ec2.Values.EbsBlockDevice.make
        ~snapshotId:snapshot_id
        ~volumeType:Ec2.Values.VolumeType.Standard
        ()
    in
    let req =
      Ec2.Values.RegisterImageRequest.make
        ~name:description
        ~rootDeviceName:device_name
        ~architecture:(Ec2.Values.ArchitectureValues.of_string architecture)
          (* FIXME: I couldn't boot an AMI instance with serial console support (nitro)
              until I enabled ENA. *)
        ~enaSupport:true
        ~sriovNetSupport:"simple"
        ~virtualizationType:"hvm"
        ~blockDeviceMappings:
          [ Ec2.Values.BlockDeviceMapping.make ~ebs ~deviceName:device_name () ]
        ()
    in
    (*
    eprintf
      "debug: register image request: %s\n"
      (req |> Ec2.Values.RegisterImageRequest.sexp_of_t |> Sexp.to_string_hum); *)
    register_image ~cfg req
  in
  let%bind () = image_available_waiter ~cfg ~image_id in
  printf !"image %s now available in %{Awsm.Region}\n" image_id region;
  return ()
;;

let main ~name ~regions ~images =
  let%bind cfg = Awsm_async.Cfg.get_exn () in
  let%bind regions =
    match regions with
    | _ :: _ -> return regions
    | [] ->
      let%bind rs = all_regions ~cfg in
      return (List.filter_map rs ~f:(fun r -> r.Ec2.Values.Region.regionName))
  in
  Deferred.List.iter images ~f:(fun image ->
    let%bind architecture = detect_architecture image in
    Deferred.List.iter regions ~how:`Parallel ~f:(fun region ->
      let cfg = { cfg with region = Some (Awsm.Region.of_string region) } in
      import_image ~cfg ~name ~architecture ~image))
;;

let () =
  let cmd =
    Command.async
      ~summary:"Import AWS EC2 image (AMI)"
      (let open Command.Let_syntax in
      let%map_open name = flag "-name" (optional string) ~doc:"NAME image name"
      and regions =
        flag "-region" (listed string) ~doc:"REGION AWS region(s) (default: all)"
      and images = anon (non_empty_sequence_as_list ("image" %: string)) in
      fun () ->
        let name =
          Option.value
            ~default:
              (sprintf "iPXE (%s)" (Date.today ~zone:Time.Zone.utc |> Date.to_string))
            name
        in
        main ~name ~regions ~images)
  in
  Command_unix.run cmd
;;
