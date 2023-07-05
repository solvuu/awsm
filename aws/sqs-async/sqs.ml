let call cfg = Awsm_async.Http.Io.call ~cfg ~service:Values.service

let dispatch_exn ~name ~f ~sexp_of_error =
  match%bind f () with
  | Ok v -> return v
  | Error (`AWS aws) ->
    failwithf "%s: %s" name (sexp_of_error aws |> Sexp.to_string_hum) ()
  | Error (`Transport err) ->
    failwithf
      "%s: transport error: %s"
      name
      (err |> Awsm.Http.Io.Error.sexp_of_call |> Sexp.to_string_hum)
      ()
;;

let unit_sexp () = Sexp.List []

let add_permission cfg ~queue_url ~label ~aws_account_ids ~actions =
  dispatch_exn ~name:"sqs.add_permission" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.add_permission
      (call cfg)
      (Values.AddPermissionRequest.make
         ~queueUrl:queue_url
         ~label
         ~aWSAccountIds:aws_account_ids
         ~actions
         ()))
;;

let change_message_visibility cfg ~queue_url ~receipt_handle ~visibility_timeout =
  dispatch_exn
    ~name:"sqs.change_message_visibility"
    ~sexp_of_error:unit_sexp
    ~f:(fun () ->
    Io.change_message_visibility
      (call cfg)
      (Values.ChangeMessageVisibilityRequest.make
         ~queueUrl:queue_url
         ~receiptHandle:receipt_handle
         ~visibilityTimeout:visibility_timeout
         ()))
;;

let change_message_visibility_batch cfg ~queue_url ~entries =
  dispatch_exn
    ~name:"sqs.message_visibility_batch"
    ~sexp_of_error:Values.ChangeMessageVisibilityBatchResult.sexp_of_error
    ~f:(fun () ->
    Io.change_message_visibility_batch
      (call cfg)
      (Values.ChangeMessageVisibilityBatchRequest.make ~queueUrl:queue_url ~entries ()))
;;

let create_queue ?attributes cfg ~queue_name =
  dispatch_exn
    ~name:"sqs.create_queue"
    ~sexp_of_error:Values.CreateQueueResult.sexp_of_error
    ~f:(fun () ->
    Io.create_queue
      (call cfg)
      (Values.CreateQueueRequest.make ?attributes ~queueName:queue_name ()))
;;

let delete_message cfg ~queue_url ~receipt_handle =
  dispatch_exn ~name:"sqs.delete_message" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.delete_message
      (call cfg)
      (Values.DeleteMessageRequest.make
         ~queueUrl:queue_url
         ~receiptHandle:receipt_handle
         ()))
;;

let delete_message_batch cfg ~queue_url ~entries =
  dispatch_exn
    ~name:"sqs.delete_message_batch"
    ~sexp_of_error:Values.DeleteMessageBatchResult.sexp_of_error
    ~f:(fun () ->
    Io.delete_message_batch
      (call cfg)
      (Values.DeleteMessageBatchRequest.make ~queueUrl:queue_url ~entries ()))
;;

let delete_queue cfg ~queue_url =
  dispatch_exn ~name:"sqs.delete_queue" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.delete_queue (call cfg) (Values.DeleteQueueRequest.make ~queueUrl:queue_url ()))
;;

let get_queue_attributes ?attribute_names cfg ~queue_url =
  dispatch_exn
    ~name:"sqs.get_queue_attributes"
    ~sexp_of_error:Values.GetQueueAttributesResult.sexp_of_error
    ~f:(fun () ->
    Io.get_queue_attributes
      (call cfg)
      (Values.GetQueueAttributesRequest.make
         ?attributeNames:attribute_names
         ~queueUrl:queue_url
         ()))
;;

let get_queue_url ?queue_owner_aws_account_id cfg ~queue_name =
  dispatch_exn
    ~name:"sqs.get_queue_url"
    ~sexp_of_error:Values.GetQueueUrlResult.sexp_of_error
    ~f:(fun () ->
    Io.get_queue_url
      (call cfg)
      (Values.GetQueueUrlRequest.make
         ?queueOwnerAWSAccountId:queue_owner_aws_account_id
         ~queueName:queue_name
         ()))
;;

let list_dead_letter_source_queues cfg ~queue_url =
  dispatch_exn
    ~name:"sqs.list_dead_letter_source_queues"
    ~sexp_of_error:Values.ListDeadLetterSourceQueuesResult.sexp_of_error
    ~f:(fun () ->
    Io.list_dead_letter_source_queues
      (call cfg)
      (Values.ListDeadLetterSourceQueuesRequest.make ~queueUrl:queue_url ()))
;;

let list_queue_tags cfg ~queue_url =
  dispatch_exn
    ~name:"sqs.list_queue_tags"
    ~sexp_of_error:Values.ListQueueTagsResult.sexp_of_error
    ~f:(fun () ->
    Io.list_queue_tags
      (call cfg)
      (Values.ListQueueTagsRequest.make ~queueUrl:queue_url ()))
;;

let list_queues ?queue_name_prefix cfg =
  dispatch_exn
    ~name:"sqs.list_queues"
    ~sexp_of_error:Values.ListQueuesResult.sexp_of_error
    ~f:(fun () ->
    Io.list_queues
      (call cfg)
      (Values.ListQueuesRequest.make ?queueNamePrefix:queue_name_prefix ()))
;;

let purge_queue cfg ~queue_url =
  dispatch_exn ~name:"sqs.purge_queue" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.purge_queue (call cfg) (Values.PurgeQueueRequest.make ~queueUrl:queue_url ()))
;;

let receive_message
  ?attribute_names
  ?message_attribute_names
  ?max_number_of_messages
  ?visibility_timeout
  ?wait_time_seconds
  ?receive_request_attempt_id
  cfg
  ~queue_url
  =
  dispatch_exn
    ~name:"sqs.receive_message"
    ~sexp_of_error:Values.ReceiveMessageResult.sexp_of_error
    ~f:(fun () ->
    Io.receive_message
      (call cfg)
      (Values.ReceiveMessageRequest.make
         ?attributeNames:attribute_names
         ?messageAttributeNames:message_attribute_names
         ?maxNumberOfMessages:max_number_of_messages
         ?visibilityTimeout:visibility_timeout
         ?waitTimeSeconds:wait_time_seconds
         ?receiveRequestAttemptId:receive_request_attempt_id
         ~queueUrl:queue_url
         ()))
;;

let remove_permission cfg ~queue_url ~label =
  dispatch_exn ~name:"sqs.remove_permission" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.remove_permission
      (call cfg)
      (Values.RemovePermissionRequest.make ~queueUrl:queue_url ~label ()))
;;

let send_message
  ?delay_seconds
  ?message_attributes
  ?message_deduplication_id
  ?message_group_id
  cfg
  ~queue_url
  ~message_body
  =
  dispatch_exn
    ~name:"sqs.send_message"
    ~sexp_of_error:Values.SendMessageResult.sexp_of_error
    ~f:(fun () ->
    Io.send_message
      (call cfg)
      (Values.SendMessageRequest.make
         ?delaySeconds:delay_seconds
         ?messageAttributes:message_attributes
         ?messageDeduplicationId:message_deduplication_id
         ?messageGroupId:message_group_id
         ~queueUrl:queue_url
         ~messageBody:message_body
         ()))
;;

let send_message_batch cfg ~queue_url ~entries =
  dispatch_exn
    ~name:"sqs.send_message_batch"
    ~sexp_of_error:Values.SendMessageBatchResult.sexp_of_error
    ~f:(fun () ->
    Io.send_message_batch
      (call cfg)
      (Values.SendMessageBatchRequest.make ~queueUrl:queue_url ~entries ()))
;;

let set_queue_attributes cfg ~queue_url ~attributes =
  dispatch_exn ~name:"sqs.set_queue_attributes" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.set_queue_attributes
      (call cfg)
      (Values.SetQueueAttributesRequest.make ~queueUrl:queue_url ~attributes ()))
;;

let tag_queue cfg ~queue_url ~tags =
  dispatch_exn ~name:"sqs.tag_queue" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.tag_queue (call cfg) (Values.TagQueueRequest.make ~queueUrl:queue_url ~tags ()))
;;

let untag_queue cfg ~queue_url ~tag_keys =
  dispatch_exn ~name:"sqs.untag_queue" ~sexp_of_error:unit_sexp ~f:(fun () ->
    Io.untag_queue
      (call cfg)
      (Values.UntagQueueRequest.make ~queueUrl:queue_url ~tagKeys:tag_keys ()))
;;
