[![CircleCI](https://circleci.com/gh/solvuu-inc/awsm/tree/master.svg?style=svg&circle-token=6e955177fec6a2e8098b21dc8decd7928b421555)](https://circleci.com/gh/solvuu-inc/awsm/tree/master)

# awsm - OCaml AWS client

Pure OCaml client for AWS. Code is auto-generated for all services
based on the API declared in
[botocore](https://github.com/boto/botocore/). Higher level functions
are often implemented on top of this base, e.g. to support multi-part
uploads to S3. Sub-libraries are provided for blocking, Async, and Lwt
versions of all code.

## Table of Contents

- [Features](#features)
- [Getting started](#getting-started)
  - [Install](#install)
  - [Examples](#examples)
- [Documentation](#documentation)
- [License](#license)
- [How to contribute](#how-to-contribute)


## Features

| Services   | unix package  | async package  | lwt package  |
| ---------- | ------------- | -------------- | ------------ |
| [Amazon Athena](https://aws.amazon.com/athena) ([doc](https://docs.aws.amazon.com/athena))         | No | Yes | No |
| [Amazon Cognito](https://aws.amazon.com/cognito) ([doc](https://docs.aws.amazon.com/cognito/latest/developerguide/amazon-cognito-integrating-user-pools-with-identity-pools.html)) | No | Yes | No |
| [Amazon EC2](https://aws.amazon.com/ec2) ([doc](https://docs.aws.amazon.com/ec2))                  | No | Yes | No |
| [Amazon ECR](https://aws.amazon.com/ecr) ([doc](https://docs.aws.amazon.com/ecr))                  | No | Yes | No |
| [Amazon Glue](https://aws.amazon.com/glue) ([doc](https://docs.aws.amazon.com/glue))               | No | Yes | No |
| [Amazon IAM](https://aws.amazon.com/iam) ([doc](https://docs.aws.amazon.com/iam))                  | No | Yes | No |
| [Amazon S3](https://aws.amazon.com/s3) ([doc](https://docs.aws.amazon.com/s3))                     | No | Yes | No |
| [Amazon SQS](https://aws.amazon.com/sqs) ([doc](https://docs.aws.amazon.com/sqs))                  | No | Yes | No |
| Cognito SRP | No | Yes | No |
| Amazon STS ([doc](https://docs.aws.amazon.com/STS/latest/APIReference)) | No | Yes | No |


## Getting started

### Install and build with local OPAM switch and lock file

Run the following commands to install a local OPAM switch based on OCaml 4.11.2 and install all package dependencies via OPAM.
(Note that after running make we must also configure the local OPAM environment.)

```
make install-deps
eval $(opam env)
```

To actually build the project you are advised to lift system restrictions on stack size,
because otherwise some files will fail to build due to stack overflows. On a modern Linux
system you can wrap the invocation of `make` under `prlimit`:

```
prlimit --stack=unlimited make
```

### Examples

Here is a short example where we use the S3 API to list the objects of the
provided bucket (see [amazon API](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-buckets.html)).

```ocaml
open Awsm_async
open! Import
open IO
module S3 = Awsm_s3.Make (IO) (Http)

let pr = Caml.print_endline

let suite_main bucket () =
  Cfg.get () >>= fun cfg ->
  S3.listBuckets cfg >>= fun _ ->
  S3.listObjects cfg (S3.ListObjectsRequest.make ~bucket ()) >>= function
  | #S3.listObjects_error -> failwith "list objects error"
  | `Ok response ->
     Option.iter response.S3.ListObjectsOutput.name ~f:pr ;
     let contents =
       Option.value ~default:[] response.S3.ListObjectsOutput.contents
     in
     let on_object oo = Option.iter (oo.S3.Object.key :> string option) ~f:pr in
     List.iter contents ~f:on_object ;
     return ()

let suite_command =
  Command.async_spec ~summary:"Test script"
    Command.Spec.(empty +> anon ("bucket" %: string))
    suite_main

let () =
  Command.group ~summary:"Awsm test app" [("test-suite", suite_command)]
  |> Command.run
```

More examples are available in the [app directory](./app).


## Documentation

The documentation is available on https://opensource.solvuu.com/docs/awsm/api

To generate the awsm API documentation locally you need `odoc`:
`opam install odoc`.

Then run `make doc`.


## License

Awsm is released under the [ISC license](./LICENSE).


## How to contribute

See [CONTRIBUTING](./CONTRIBUTING.md) for how to help out.
