open Core
open Values

type t =
  { number : int
  ; start_offset : int64
  ; size : int64
  }
[@@deriving sexp_of]

let number p = p.number + 1

let cons_maybe hdo tl =
  match hdo with
  | Some hd -> hd :: tl
  | None -> tl
;;

let build_parts_from_size ~part_size size =
  let open Int64 in
  let num_parts = size / part_size in
  let part_start_offset n = n * part_size in
  let last_part_size = rem size part_size in
  let last_part_opt =
    if last_part_size = 0L
    then None
    else
      Some
        { number = to_int_exn num_parts
        ; start_offset = part_start_offset num_parts
        ; size = last_part_size
        }
  in
  List.range' ~compare:Int64.compare ~stride:Int64.succ 0L num_parts
  |> List.rev_map ~f:(fun part_num ->
       let size = part_size in
       { start_offset = part_start_offset part_num; size; number = to_int_exn part_num })
  |> cons_maybe last_part_opt
  |> List.rev
;;

let%expect_test "build_parts_from_size" =
  let test ~part_size size =
    build_parts_from_size ~part_size size |> [%sexp_of: t list] |> print_s
  in
  test ~part_size:10L 30L;
  [%expect
    {|
    (((number 0) (start_offset 0) (size 10))
     ((number 1) (start_offset 10) (size 10))
     ((number 2) (start_offset 20) (size 10))) |}];
  test ~part_size:10L 34L;
  [%expect
    {|
    (((number 0) (start_offset 0) (size 10))
     ((number 1) (start_offset 10) (size 10))
     ((number 2) (start_offset 20) (size 10))
     ((number 3) (start_offset 30) (size 4))) |}];
  test ~part_size:10L 0L;
  [%expect {| () |}];
  test ~part_size:10L 1L;
  [%expect {| (((number 0) (start_offset 0) (size 1))) |}]
;;

let build_parts path =
  let stat = Core_unix.stat path in
  build_parts_from_size ~part_size:0x500000L stat.st_size
;;

type params =
  { bucket : string
  ; key : string
  ; uploadId : string
  }

let params_of_creation { CreateMultipartUploadOutput.bucket; key; uploadId; _ } =
  let message s =
    Printf.sprintf "missing field in multipart upload creation output: %s" s
  in
  { bucket = Option.value_exn ~message:(message "bucket") bucket
  ; key = Option.value_exn ~message:(message "key") key
  ; uploadId = Option.value_exn ~message:(message "uploadId") uploadId
  }
;;

let upload_request ~creation ~path part =
  let { bucket; key; uploadId } = params_of_creation creation in
  let partNumber = number part in
  let body_s =
    In_channel.with_file path ~f:(fun ic ->
      In_channel.seek ic part.start_offset;
      let len = Int64.to_int_exn part.size in
      Stdlib.really_input_string ic len)
  in
  let body = Body.of_string body_s in
  UploadPartRequest.make ~bucket ~key ~partNumber ~uploadId ~body ()
;;

let completed_part part { UploadPartOutput.eTag; _ } =
  let partNumber = number part in
  let eTag = Core.Option.value_exn ~message:"no ETag in output" eTag in
  CompletedPart.make ~eTag ~partNumber ()
;;

let complete_request ~creation ~parts =
  let { bucket; key; uploadId } = params_of_creation creation in
  let multipartUpload = CompletedMultipartUpload.make ~parts () in
  CompleteMultipartUploadRequest.make ~multipartUpload ~bucket ~key ~uploadId ()
;;
