open Awsm_sqs_async

let main () =
  let%bind cfg = Awsm_async.Cfg.get_exn () in
  let queue_name = "my-foo-test-queue" in
  printf "creating queue: %s\n" queue_name;
  let%bind res = Sqs.create_queue cfg ~queue_name in
  printf
    "create_queue result: %s\n"
    (res |> Values.CreateQueueResult.sexp_of_t |> Sexp.to_string_hum);
  let queue_url =
    res.Values.CreateQueueResult.createQueueResult.queueUrl
    |> Option.value_exn ~message:"No queueUrl"
  in
  printf "listing queues\n";
  let%bind res = Sqs.list_queues cfg in
  let lqr = res.Values.ListQueuesResult.listQueuesResult in
  Option.iter lqr.Values.ListQueuesResult.queueUrls ~f:(fun queue_urls ->
    List.iter queue_urls ~f:(fun url -> printf "- %s\n" url));
  let message_body = sprintf !"Test message body %{Time_unix}" (Time.now ()) in
  printf "sending message '%s' to queue %s\n" message_body queue_url;
  let%bind res = Sqs.send_message cfg ~queue_url ~message_body in
  printf
    "send_message result: %s\n"
    (res |> Values.SendMessageResult.sexp_of_t |> Sexp.to_string_hum);
  printf "receive_message: %s\n" queue_url;
  let%bind res = Sqs.receive_message cfg ~queue_url in
  printf
    "receive_message result: %s\n"
    (res |> Values.ReceiveMessageResult.sexp_of_t |> Sexp.to_string_hum);
  let%bind () = Sqs.delete_queue cfg ~queue_url in
  printf "queue %s deleted\n" queue_url;
  return ()
;;

let () =
  don't_wait_for
    (Monitor.try_with (fun () -> main ())
    >>= fun res ->
    match res with
    | Ok () ->
      Shutdown.shutdown 0;
      return ()
    | Error e ->
      eprintf "error: %s\n" (Exn.to_string e);
      Shutdown.shutdown 1;
      return ());
  never_returns (Scheduler.go ())
;;
