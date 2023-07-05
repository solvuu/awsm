(** AWS regions. *)
open! Core

open! Import

type t = private string [@@deriving sexp, compare]

val of_string : string -> t
val to_string : t -> string

(* Asia Pacific *)
val ap_northeast_1 : t
val ap_northeast_2 : t
val ap_northeast_3 : t
val ap_south_1 : t
val ap_southeast_1 : t
val ap_southeast_2 : t

(* Canada *)
val ca_central_1 : t

(* China *)
val cn_north_1 : t
val cn_northwest_1 : t

(* EU *)
val eu_central_1 : t
val eu_north_1 : t
val eu_west_1 : t
val eu_west_2 : t
val eu_west_3 : t

(* South America *)
val sa_east_1 : t

(* US *)
val us_east_1 : t
val us_east_2 : t
val us_west_1 : t
val us_west_2 : t

(* AWS GovCloud *)
val us_gov_east_1 : t
val us_gov_west_1 : t
val all : t list
