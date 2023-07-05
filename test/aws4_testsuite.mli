(** Parsers for files in aws4_testsuite. *)
open! Import

module Req : sig
  (** [of_file filename] parses the given ".req" file. We correct for the
      following error in AWS's files:

      - They write header lines with lowercase "http". Unsure if this is
      disallowed by the HTTP protocol, but it certainly isn't standard and not
      supported by Cohttp. We simply capitalize this part. *)
  val of_file : string -> Cohttp.Request.t * Cohttp.Body.t
end

module Creq : sig
  (** [of_file filename] parses the given ".creq" file. We coorrect for
      following error in AWS's files:

      - Lines end with '/r/n' but their spec states that '\n' should be used. *)
  val of_file : string -> string
end
