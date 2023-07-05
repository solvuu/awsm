# AWSM

La reconstruction du projet s'articule autour du problème que la
_functorization_ des modules implémentant les APIs amazon est impossible. Dans
ce contexte, il faut trouver une autre manière pour avoir le même niveau
d'abstraction sans passer par un _functor_.

## Type IO

La grosse problématique se concentre sur notre capacité à pouvoir abstraire le
type: `Async.t` ou `Lwt.t`. Ces derniers sont des types paramétriques, une
abstraction _à la haskell_ requiert le _high-kind polymorphism_ et ainsi avoir
une abstraction: `'a 'b` où `'b` respecterait certaines contraintes.

Jeremie Yallop a fait une librairie, `higher`, permettant de _simuler_ le
_high-kind polymorphism_ en OCaml (le système de type d'OCaml n'étant pas
capable de gérer ce cas).

```ocaml
type ('a, 's) apply

module type S = sig
  type 'a s
  type t
  
  val inj : 'a s -> ('a, t) apply
  val prj : ('a, t) apply -> 'a t
end

module type FUNCTOR = sig type 'a t end

module Make (T : FUNCTOR) : S with type 'a s = 'a T.t = struct
  type 'a s = 'a T.t
  type t
  
  external inj : 'a s -> ('a, t) apply = "%identity"
  external prj : ('a, t) apply -> 'a s = "%identity"
end
```

Ce code consiste à créer un nouveau type `t` qui représenterait le `'b` dans
l'exemple `'a 'b`. La construction de ce type `t` ce fera par rapport à `T : FUNCTOR`
qui, dans le cas qui nous intéresse, serait `Async : FUNCTOR` ou `Lwt : FUNCTOR`.

Ensuite, on a les fonctions d'injection et de projection pour pouvoir passer de
`'a 'b` a `('a, t) apply` et vice-versa. L'implémentation de ces fonctions est
`%identity` car cette autre représentation ne veut mentir qu'auprès du système
de type et non pas sur l'exécution du code (et sa représentation physique).

La capacité d'assimiler un nouveau type `('a, 's) apply` ou plus précisement
`('a, 's) io` dans notre cas nous permet de nous abstraire de ce que peut
représenter `'s` qui peut être `Lwt.t` ou `Async.t`.

Un code abstrait de l'IO - comme objectif initial - sans les _functors_
ressemblerait à:

```ocaml
let return : 'a -> ('a, 's) io (* Lwt.return : 'a -> 'a Lwt.t *)
```

Bien entendu, l'abstration du type IO n'est pas suffisante. À cette étape, nous
n'avons la capacité d'avoir que cette équivalence: 

```ocaml
module Make (IO : sig type 'a t end) = struct
  type 'a io = 'a IO.t
end
```

Il s'agit aussi d'obtenir les fonctions usuelles au module `IO`, c'est à dire
les implémentations:

```ocaml
type 's impl =
  { bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return : 'a. 'a -> ('a, 's) io }
```

Cette structure est très proche de ce que devrait être le module `IO`:

```ocaml
module type IO = sig
  type 'a t
  
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end
```

À la différence où l'on utilise `type ('a, 's) io` en lieu et place de `type 'a IO.t`.
Puisque le choix a été fait de ne pas _functorizer_ le code, il s'agit de donner
l'implémentation `'s impl` à chaque processus intéragissant avec _l'extérieur_
(_syscall_, requête HTTP dans notre exemple final).

Ces processus devront s'écrire de cette manière:

```ocaml
let f 
  : type s. s impl -> ... -> (x, s) io
  = fun impl ... ->
    let return = impl.return in
    let ( >>= ) = impl.bind in

    return ...
```

Ici, on ne fait que reproduire ce que un _functor_ fait en réalité, le point est
que nous le faisons à la main et que l'application partielle de `f` avec une
`impl` spécifique est semblable à l'application d'un _functor_ avec un module
`IO` spécifique (comme `Async` ou `Lwt`).

### Exemple avec le type IO

`ocaml-dkim` utilise ce type d'abstraction. La glue nécessaire se fait en dehors
de la librairie et assure à ce qu'on ne dépende pas de `Lwt` ou de `Async` ou
même de `Unix` (qui consiste à `type ('a, unix) = 'a`).

L'implémentation de `prj` et `inj`, en étant l'`identity`, permet d'évité un
_overhead_ dans les performances. Cela n'a pas été démontré mais l'application
d'un _functor_ (puisque sa construction peut avoir un effet de bord) rajoute un
argument fantôme à toutes les fonctions construite - ce qui peut être considéré
comme un _overhead_.

Néanmoins, `prj` et `inj` peut être fastidieux à utiliser surtout si on veut
passer de `'a -> ('a, lwt) io` à `'a -> 'a Lwt.t` à la main.

### Organisation avec AWSM

Le project `awsm` a une sous-librairie `base` qui devrait avoir ce type `io` et
pourrait être utiliser dans l'implémentation des APIs. Le module `Sigs`
implémente le _functor_ permettant de créer ce nouveau type `t` et décrit `impl`
(dans le code, le type ce nomme `state` mais le nom n'est pas judicieux).

L'implémentation et la génération du code pour les APIs amazon devrait dépendre
de ce type `io` comme expliqué ci-dessus.

## API amazon

Dans le status courrant du projet `awsm`, la misconception qui a aboutit à
l'erreur expliqué en introduction est l'utilisation de `bind` et `return` dans
la construction de valeurs _caml_ n'ayant pas besoin d'interaction avec un
_syscall_ (ex: construction de l'`Uri.t`).

La construction des requêtes qui ensuite doivent être envoyé à l'aide d'un
client HTTP est en dehors du scope d'un _scheduler_ comme `Async` ou `Lwt`.
Toutes ces valeurs peuvent être créé seulement avec OCaml.

Une API amazon (comme s3) ne devrait dépendre de `type ('a, 's) io` seulement
lorsqu'il s'agira d'_évaluer_ une description définissant:
- à qu'elle _endpoint_ de l'API nous voulons communiquer
- avec qu'elle valeurs

On peut déjà imaginer la fonction `eval`:

```ocaml
val eval : endpoint -> request -> (response, 's) io
```

Il y a bien entendu un lein entre `endpoint` et `request`. Chaque `endpoint`
d'une API requiert l'envoie d'un certain type de valeur. Il en est de même pour
la reponse qui dépends aussi du `endpoint`:

```ocaml
val eval : ('i, 'o) endpoint -> 'i -> ('o, 's) io
```

### Endpoint

En cela, en lieu et place de générer des fonctions communiquant directement avec
l'API (et donc forcément assujeties au type `io`), il s'agit d'offrir un moyen
de décrire cette communication au travers du type `endpoint` et finalement
fournir une fonction `eval` abstraite de la manière dont la requête devrait se
construire et la faćon dont on devrait traiter la reponse. L'idée est de générer
un GADT décrivant tout les `endpoints` possibles d'une API:

```ocaml
type ('i, 'o) endpoint =
  | A : (ARequest.t, AOutput.t) endpoint
  | B : (BRequest.t, BOutput.t) endpoint
```

De ces `endpoints`, il s'agit d'implémenter 4 fonctions:
- `to_request : ('i, 'o) endpoint -> 'i -> Request.t`
- `to_uri : ('i, 'o) endpoint -> 'i -> Uri.t`
- `to_meth : ('i, 'o) endpoint -> Meth.t'`
- `of_response : ('i, 'o) endpoint -> Response.t -> 'o`

Comme il a été dit, ces implémentations sont déjà disponible mais elle utilise
le type `'a IO.t` alors qu'il ne s'agit essentiellement que de créer des valeurs
_caml_. 

L'objectif est donc d'extraire ces générations de _botocore_ afin qu'elles
soient indépendantes de `'a IO.t`.

#### Request and Response

Bien entendu, l'existance de `Request.t` et `Response.t` (avec `Meth.t` pour les
méthodes HTTP et `Headers.t` pour le _headers_ HTTP) seront communes à toutes
les API et leurs définitions devrait être dans la sous-librairie `base` - comme
c'est le cas dans le _proof-of-concept_.

L'implémentation de ces types est sans difficulté et devrait être un tronc
commun entre `cohttp` et `httpaf`.

#### Botocore

`botocore` génère déjà tout le code nécessaire. Le seul problème est que l'étape
entre générer la requête, envoyer la requête et ensuite traiter la reponse,
toutes ces étapes sont sous une même fonction. Il s'agit essentiellement de
_splitter_ le code généré en ces 4 fonctions et la fonction `eval` serait alors
une composition de ces 4 fonctions pour ensuite réellement exécuter `HTTP.call`
(envoyer la requête HTTP).

À ce niveau, il n'y a toujours pas de notion d'`IO`.

## Spécialisation par rapport au _backend_

Le projet `awsm` va devoir spécialiser différents _backend_ (`Lwt`, `Async`,
`Unix`) pour réutiliser le `type 's impl`/`type 's state` et être dans la
capacité de produire directement la fonction `eval` pour les APIs.

Ainsi, on devrait avoir:
- `awsm-async`
- `awsm-lwt`
- `awsm-unix`

Le code de ces sous-paquets est trivial où il s'agit surtout d'appliquer le
_functor_ vu à la première partie et ensuite d'implémenter `type 's impl`/`type 's state`.
Voici un exemple pour le _backend_ Async:

```ocaml
module Async_scheduler = Make(struct type +'a t = 'a Async.Deferred.t end)

let async_bind x f =
  let open Async_scheduler in
  let open Async.Deferred in
  let  ( <.> ) = Core.Fn.compose in
  inj (prj x >>= (prj <.> f))

let async_return x =
  Async_scheduler.inj (Async.Deferred.return x)

let async =
  let open Poc_awsm_base.Sigs in
  { bind= async_bind
  ; return= async_return }

type async = Async_scheduler.t
let inj : 'a Async.Deferred.t -> ('a, async) Poc_awsm_base.Sigs.io = Async_scheduler.inj
let prj : ('a, async) Poc_awsm_base.Sigs.io -> 'a Async.Deferred.t = Async_scheduler.prj
```

## Composition entre _backend_ et API, abstraction du client HTTP

Enfin, en étape finale, il s'agit enfin de faire la fonction `eval` par rapport
à un _backend_ (nous allons prendre le _backend_ UNIX où `type 'a t = 'a`). Nous
avons donc le type `('i, 'o) endpoint` par rapport à l'API (comme l'API S3), les
fonctions:
- `to_meth : ('i, 'o) endpoint -> Meth.t`
- `to_request : ('i, 'o) endpoint -> Request.t`
- `to_uri : ('i, 'o) endpoint -> Uri.t`
- `of_response : ('i, 'o) endpoint -> Response.t -> 'o`

Il s'agira aussi d'avoir une fonction (qui, elle, dépends de l'IO) permettant de
communiquer avec une API HTTP:

```ocaml
val http_call : meth:Meth.t -> Request.t -> Uri.t -> (Response.t, 's) io
```

Une spécialisation par rapport aux _backend_ est possible avec:

```ocaml
val http_unix_call : meth:Meth.t -> Request.t -> Uri.t -> Response.t

let http_call ~meth req uri =
  let res = http_unix_call ~meth req uri in
  Awsm_unix.inj res
```

Enfin la fonction `eval` devrait avoir cette signature:

```ocaml
type 's http = meth:Meth.t -> Request.t -> Uri.t -> (Response.t, 's) io
type http_unix = Awsm_unix.t http

val eval : http_unix -> ('i, 'o) endpoint -> 'i -> 'o
```

Le type `'s http` peut se définir dans `awsm-base` puisqu'il ne dépends que de
`('a, 's) io`. Le type `http_unix` peut se définir dans `Awsm_unix` puisqu'il
est une spécialisation de `'s http` avec le type `Awsm_unix.t` (le type
frechement créé par le _functor_ `Make`).

Finalement, son implémentation devrait être très mécanique (et même commune à toutes les APIs):

```ocaml
open Awsm_unix

let eval
  : type i o. http_unix - (i, o) S3.endpoint -> i -> o
  = fun http edn i ->
    let meth = S3.to_meth edn i in
    let req = S3.to_request edn i in
    let uri = S3.to_uri edn i in
    let res = http ~meth req uri in
    S3.of_response edn (prj res)
```

## Obtenir la même interface que AWSM

Enfin, en dernière étape facultative et facilement générable, elle consiste à
spécialiser `eval` avec chaque `endpoints` de l'API:

```ocaml
Awsm_s3:
type ('i, 'o) endpoint =
  | SendObject : (ObjectRequest.t, ObjectOutput.t) endpoint
  
Awsm_s3_unix:
let send_object
  : ObjectRequest.t -> ObjectOutput.t
  = eval http_unix SendObject
```
