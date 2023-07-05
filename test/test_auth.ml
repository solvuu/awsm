open! Import
include Awsm.Auth

(* 2022-09-13 mbac: Some auth tests from the standard test suite are skipped because
  they require implementing some rather unusual "canonical header" encoding/handling 
  that we ourselves wouldn't sanely generate. For these cases we'll adopt a 'wait and see'
  approach as to whether or not they need to be implemented in practice. *)
let ok ~name = Test.pass "aws testsuite" [ "name", name ]

let skip ~name reason =
  Test.fail "aws testsuite" [ "name", name; "skipped", "true"; "reason", reason ]
;;

let err ~name ~expected ~got =
  Test.fail "aws testsuite" [ "name", name; "expected", expected; "got", got ]
;;

type test =
  { name : string
  ; req_file : string
  ; creq_file : string
  }

let test_canonical_request { name; req_file; creq_file } =
  if List.mem
       ~equal:String.equal
       [ "get-vanilla-with-session-token"
       ; "get-vanilla-query-order-encoded"
       ; "get-header-value-trim"
       ; "get-header-value-multiline"
       ; "post-vanilla-query-space"
       ; "post-vanilla-query-nonunreserved"
       ]
       name
  then skip ~name "test is not supported yet, see comment at top of test/test_auth.ml"
  else (
    let request, body = Aws4_testsuite.Req.of_file req_file in
    let body = Cohttp.Body.to_string body in
    let payload_hash = `Signed (payload_hash body) in
    let expected = Aws4_testsuite.Creq.of_file creq_file in
    let computed =
      let headers = Cohttp.Request.headers request in
      canonical_request
        ~http_method:(Cohttp.Request.meth request)
        ~uri:(Cohttp.Request.uri request)
        ~headers
        ~payload_hash
    in
    let got = (computed :> string) in
    if String.equal expected got then ok ~name else err ~name ~expected ~got)
;;

let make_test ~dir name =
  let ( / ) = Filename.concat in
  let req_file = dir / (name ^ ".req") in
  let creq_file = dir / (name ^ ".creq") in
  { name; req_file; creq_file }
;;

let to_test_name s =
  match Filename.split_extension s with
  | name, Some "req" -> Some name
  | _ -> None
;;

let discover_tests ~dir =
  Sys_unix.readdir dir
  |> Array.to_list
  |> List.sort ~compare:String.compare
  |> List.filter_map ~f:to_test_name
  |> List.map ~f:(make_test ~dir)
;;

let () =
  let dir = (Sys.get_argv ()).(1) in
  let test_names = discover_tests ~dir in
  List.iter test_names ~f:test_canonical_request
;;
