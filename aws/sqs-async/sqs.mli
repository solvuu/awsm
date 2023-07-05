open Awsm_async
open! Import

val add_permission
  :  Awsm.Cfg.t
  -> queue_url:string
  -> label:string
  -> aws_account_ids:string list
  -> actions:string list
  -> unit Deferred.t

val change_message_visibility
  :  Awsm.Cfg.t
  -> queue_url:string
  -> receipt_handle:string
  -> visibility_timeout:int
  -> unit Deferred.t

val change_message_visibility_batch
  :  Awsm.Cfg.t
  -> queue_url:string
  -> entries:Values.ChangeMessageVisibilityBatchRequestEntry.t list
  -> Values.ChangeMessageVisibilityBatchResult.t Deferred.t

val create_queue
  :  ?attributes:(Values.QueueAttributeName.t * string) list
  -> Awsm.Cfg.t
  -> queue_name:string
  -> Values.CreateQueueResult.t Deferred.t

val delete_message
  :  Awsm.Cfg.t
  -> queue_url:string
  -> receipt_handle:string
  -> unit Deferred.t

val delete_message_batch
  :  Awsm.Cfg.t
  -> queue_url:string
  -> entries:Values.DeleteMessageBatchRequestEntry.t list
  -> Values.DeleteMessageBatchResult.t Deferred.t

val delete_queue : Awsm.Cfg.t -> queue_url:string -> unit Deferred.t

val get_queue_attributes
  :  ?attribute_names:Values.QueueAttributeName.t list
  -> Awsm.Cfg.t
  -> queue_url:string
  -> Values.GetQueueAttributesResult.t Deferred.t

val get_queue_url
  :  ?queue_owner_aws_account_id:string
  -> Awsm.Cfg.t
  -> queue_name:string
  -> Values.GetQueueUrlResult.t Deferred.t

val list_dead_letter_source_queues
  :  Awsm.Cfg.t
  -> queue_url:string
  -> Values.ListDeadLetterSourceQueuesResult.t Deferred.t

val list_queue_tags
  :  Awsm.Cfg.t
  -> queue_url:string
  -> Values.ListQueueTagsResult.t Deferred.t

val list_queues
  :  ?queue_name_prefix:string
  -> Awsm.Cfg.t
  -> Values.ListQueuesResult.t Deferred.t

val purge_queue : Awsm.Cfg.t -> queue_url:string -> unit Deferred.t

val receive_message
  :  ?attribute_names:Values.QueueAttributeName.t list
  -> ?message_attribute_names:string list
  -> ?max_number_of_messages:int
  -> ?visibility_timeout:int
  -> ?wait_time_seconds:int
  -> ?receive_request_attempt_id:string
  -> Awsm.Cfg.t
  -> queue_url:string
  -> Values.ReceiveMessageResult.t Deferred.t

val remove_permission : Awsm.Cfg.t -> queue_url:string -> label:string -> unit Deferred.t

val send_message
  :  ?delay_seconds:int
  -> ?message_attributes:(string * Values.MessageAttributeValue.t) list
  -> ?message_deduplication_id:string
  -> ?message_group_id:string
  -> Awsm.Cfg.t
  -> queue_url:string
  -> message_body:string
  -> Values.SendMessageResult.t Deferred.t

val send_message_batch
  :  Awsm.Cfg.t
  -> queue_url:string
  -> entries:Values.SendMessageBatchRequestEntry.t list
  -> Values.SendMessageBatchResult.t Deferred.t

val set_queue_attributes
  :  Awsm.Cfg.t
  -> queue_url:string
  -> attributes:(Values.QueueAttributeName.t * string) list
  -> unit Deferred.t

val tag_queue
  :  Awsm.Cfg.t
  -> queue_url:string
  -> tags:(string * string) list
  -> unit Deferred.t

val untag_queue
  :  Awsm.Cfg.t
  -> queue_url:string
  -> tag_keys:string list
  -> unit Deferred.t
