# AWSM

NOTE: This was semi-robotically translated from French to English. [The original is here](plan_original_french.md).

NOTE: Important background reading! [Lightweight higher-kinded polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf) by Yallop and White.

The refactoring of the project revolves around the problem that the
_functorization_ of modules implementing amazon APIs is impossible. In
this context, it is necessary to find another way to have the same level
of abstraction without going through a _functor_.

## IO type

The main problem focuses on our ability to be able to abstract the
type: `Async.t` or `Lwt.t`. The latter are parametric types; an
abstraction _à la haskell_ requires _high-kind polymorphism_ and thus has
an abstraction: `'a 'b` where `'b` would respect some constraints.

Jeremie Yallop made a library, `higher`, allowing one to _simulate_ the
_high-kind polymorphism_ in OCaml (the OCaml type system not being
able to handle this case).

```ocaml
type('a,'s) apply

module type S = sig
  type 'a s
  type t

  val inj: 'a s -> ('a, t) apply
  val prj: ('a, t) apply -> 'a t
end

module type FUNCTOR = sig type 'a t end

module Make (T: FUNCTOR): S with type 'a s = 'a T.t = struct
  type 'a s = 'a T.t
  type t

  external inj: 'a s -> ('a, t) apply = "%identity"
  external prj: ('a, t) apply -> 'a s = "%identity"
end
```

This code consists of creating a new type `t` which would represent the `'b` in
example `'a 'b`. The construction of this type `t` can be compared to `T:FUNCTOR`
which, in our case, would be `Async:FUNCTOR` or `Lwt:FUNCTOR`.

Then, we have the injection and projection functions to be able to go from
`'a 'b` to `('a, t) apply` and vice versa. The implementation of these functions is
`%identity` because that other representation only wants to lie to the type system and not to the execution of the code (and its physical representation)

?? not sure on the above, need to check the paper ??

The ability to assimilate a new type `('a, 's) apply` or more precisely
`('a, 's) io` in our case, allows us to abstract ourselves from what can
represent `'s` which can be `Lwt.t` or `Async.t`.

An abstract IO code - as an initial goal - without the _functors_
would look like:

```ocaml
let return: 'a -> ('a, 's) io (* Lwt. return: 'a -> 'a Lwt.t *)
```

Of course, IO type abstraction is not enough. At this stage, we
we only have the ability to have this equivalence:

```ocaml
module Make(IO: sig type 'a t end) = struct
  type 'a io = 'a IO.t
end
```

There is also the question of obtaining the usual functions in the `IO` module, i.e.
implementations:

```ocaml
type 's impl =
  { bind: 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return: 'a. 'a -> ('a, 's) io }
```

This structure is very close to what the `IO` module should be:

```ocaml
module type IO = sig
  type 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end
```

Unlike where we use `type ('a, 's) io` instead of `type 'a IO.t`.
Since the choice was made not to _functorize_ the code, it is a matter of giving
the implementation `'s impl` to each process interacting with _the outside_
(_syscall_, HTTP request in our final example).

These processes should be written like this:

```ocaml
let f
  : type s. s impl -> ... -> (x, s) io
  = fun impl... ->
    let return = impl.return in
    let ( >>= ) = impl.bind in

    return...
```

Here we're just replicating what a _functor_ actually does - the point is
that we do it by hand and partial application of `f` with a
specific `impl` is similar to applying a _functor_ with a module
Specific `IO` (like `Async` or `Lwt`).

### Example with IO type

`ocaml-dkim` uses this kind of abstraction. The necessary glue is done outside
of the library and ensures that we do not depend on `Lwt` or `Async` or
even from `Unix` (which consists of `type ('a, unix) = 'a`).

The implementation of `prj` and `inj`, being the `identity`, avoids an
_overhead_ in performance. This has not been demonstrated but the application
of a _functor_ (since its construction can have side effects) adds a
phantom argument to all constructed functions - which can be considered an _overhead_.

However, `prj` and `inj` can be tedious to use, especially if you want to
switch from `'a -> ('a, lwt) io` to `'a -> 'a Lwt.t` by hand.

### Organizing with AWSM

The `awsm` project has a `base` sublibrary which should have this type `io` and
could be used in the implementation of APIs. The `Sigs` module
implements the _functor_ to create this new type `t` and describes `impl`
(in the code, the type is called `state` but the name is not appropriate).

Implementation and code generation for amazon APIs should depend
on this `io` type as explained above.

## Amazon API

In the current status of the `awsm` project, the misconception that resulted in
the error explained in the introduction is the use of `bind` and `return` in
constructing _caml_ values ​​that do not need interaction with a
_syscall_ (ex: construction of the `Uri.t`).

The construction of requests which then must be sent using an
HTTP client is outside the scope of a _scheduler_ like `Async` or `Lwt`.
All these values ​​can be created only with OCaml.

An Amazon API (like s3) should depend on `type('a,'s) io` only
when it comes to _evaluating_ a description defining:
- which _endpoint_ of the API we want to communicate with
- with what values

We can already imagine the `eval` function:

```ocaml
val eval: endpoint -> request -> (response, 's) io
```

There is of course a link between `endpoint` and `request`. Each `endpoint`
of an API requires the sending of a certain type of value. It is the same for
the response, which also depends on the `endpoint`:

```ocaml
val eval: ('i, 'o) endpoint -> 'i -> ('o, 's) io
```

### Endpoint

In this, instead of generating functions communicating directly with
the API (and therefore necessarily subject to the `io` type), it is a question of offering a means
to describe this communication through the `endpoint` type and finally,
provide an abstract `eval` function of how the query should proceed and how the response should be handled. The idea is to generate
a GADT describing all the possible `endpoints` of an API:

```ocaml
type('i, 'o) endpoint=
  | A: (ARequest.t, AOutput.t) endpoint
  | B: (BRequest.t, BOutput.t) endpoint
```

Of these `endpoints`, it is a question of implementing 4 functions:
- `to_request: ('i, 'o) endpoint -> 'i -> Request.t`
- `to_uri: ('i, 'o) endpoint -> 'i -> Uri.t`
- `to_meth: ('i, 'o) endpoint -> Meth.t'`
- `of_response: ('i, 'o) endpoint -> Response.t -> 'o`

As stated, these implementations are already available but they use
type 'a IO.t' which is essentially just creating values in _caml_.

The objective is therefore to extract these generations of _botocore_ so that they
are independent of `'a IO.t`.

#### Request and Response

Of course, the existence of `Request.t` and `Response.t` (with `Meth.t` for
HTTP and `Headers.t` methods for the HTTP _headers_) will be common to all
APIs and their definitions should be in the `base` sublibrary - as
this is the case in the _proof-of-concept_.

Implementing these types should be easy and should share a common trunk (??) between `cohttp` and `httpaf`.

#### Botocore

`botocore` already generates all the necessary code. The only problem is that the step
between generating the request, sending the request and then processing the response,
is all under one function. It is necessary to _split_ the generated code into these 4 functions, and the `eval` function would then be
a combination of these 4 functions to then actually execute `HTTP.call`
(send the HTTP request).

At this level, there is still no notion of `IO`.

## Specialization relative to the _backend_

The `awsm` project will have to specialize different _backends_ (`Lwt`, `Async`,
`Unix`) to reuse the `type 's impl`/`type 's state` and have the
ability to directly produce the `eval` function for APIs.

Thus, we should have:
- `awsm-async`
- `awsm-lwt`
- `awsm-unix`

The code for these subpackages is trivial where it is mostly a matter of applying the
_functor_ seen in part one and then to implement `type 's impl`/`type 's state`.
Here is an example for the Async _backend_:

```ocaml
module Async_scheduler = Make(struct type +'a t = 'a Async.Deferred.t end)

let async_bind x f =
  let open Async_scheduler in
  let open Async.Deferred in
  let (<.>) = Core.Fn.compose in
  inj (prj x >>= (prj <.> f))

let async_return x=
  Async_scheduler.inj (Async.Deferred.return x)

let async=
  let open Poc_awsm_base.Sigs in
  { bind= async_bind
  ; return= async_return }

type async = Async_scheduler.t
let inj: 'a Async.Deferred.t -> ('a, async) Poc_awsm_base.Sigs.io = Async_scheduler.inj
let prj: ('a, async) Poc_awsm_base.Sigs.io -> 'a Async.Deferred.t = Async_scheduler.prj
```

## Composition between _backend_ and API, HTTP client abstraction

Lastly, in the final step, it is a question of making the function `eval` with respect to
to a _backend_ (we'll take the UNIX _backend_ where `type 'a t = 'a`). We have the type `('i, 'o) endpoint` against the API (like the S3 API), and the
functions:
- `to_meth: ('i, 'o) endpoint -> Meth.t`
- `to_request: ('i, 'o) endpoint -> Request.t`
- `to_uri: ('i, 'o) endpoint -> Uri.t`
- `of_response: ('i, 'o) endpoint -> Response.t -> 'o`

It will also be a question of having a function (which itself depends on the IO) making it possible to communicate with an HTTP API:

```ocaml
val http_call: meth:Meth.t -> Request.t -> Uri.t -> (Response.t, 's) io
```

Specialization with respect to _backends_ is possible with:

```ocaml
val http_unix_call: meth:Meth.t -> Request.t -> Uri.t -> Response.t

let http_call ~meth req uri=
  let res = http_unix_call ~meth req uri in
  Awsm_unix.inj res
```

Finally the `eval` function should have this signature:

```ocaml
type 's http = meth:Meth.t -> Request.t -> Uri.t -> (Response.t, 's) io
type http_unix = Awsm_unix.t http

val eval: http_unix -> ('i, 'o) endpoint -> 'i -> 'o
```

The `'s http` type can be defined in `awsm-base` since it only depends on
`('a, 's) io`. The `http_unix` type can be defined in `Awsm_unix` since it
is a specialization of `'s http` with type `Awsm_unix.t` (the type
freshly created by _functor_ `Make`).

Finally, its implementation should be very mechanical (and even common to all APIs):

```ocaml
open Awsm_unix

let eval
  : type i o. http_unix - (i, o) S3.endpoint -> i -> o
  = fun http edn i ->
    let meth = S3.to_meth edn i in
    let req = S3.to_request edn i in
    let uri = S3.to_uri edn i in
    let res = http ~meth req uri in
    S3.of_response edn(prj res)
```

## Get the same interface as AWSM

Finally, the last optional and easily generated step, consists of
specializing `eval` with each API `endpoints`:

```ocaml
Awsm_s3:
type('i, 'o) endpoint=
  | SendObject: (ObjectRequest.t, ObjectOutput.t) endpoint

Awsm_s3_unix:
let send_object
  : ObjectRequest.t -> ObjectOutput.t
  = eval http_unix SendObject
```
