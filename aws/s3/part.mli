open Values

type t

val build_parts : string -> t list

val upload_request
  :  creation:CreateMultipartUploadOutput.t
  -> path:string
  -> t
  -> UploadPartRequest.t

val completed_part : t -> UploadPartOutput.t -> CompletedPart.t

val complete_request
  :  creation:CreateMultipartUploadOutput.t
  -> parts:CompletedPartList.t
  -> CompleteMultipartUploadRequest.t
