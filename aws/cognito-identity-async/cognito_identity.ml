let default_max_results = 20

let list_identity_pools cfg ?(max_results = default_max_results) ()
  : Values.IdentityPoolShortDescription.t list Deferred.t
  =
  let maxResults = Values.QueryLimit.make max_results in
  Io.list_identity_pools
    (Awsm_async.Http.Io.call ~cfg ~service:Values.service)
    (Values.ListIdentityPoolsInput.make ~maxResults ())
  >>| function
  | Ok response -> Option.value response.identityPools ~default:[]
  | _ -> failwithf "list_identity_pools error" ()
;;

let identity_pools_to_json t = Values.IdentityPoolsList.to_json t
let identity_pools_to_string t = t |> identity_pools_to_json |> Awsm.Json.to_string
