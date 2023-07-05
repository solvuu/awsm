open! Import

val lookup_credential_scope : region:Region.t -> Service.t -> Region.t
val lookup_uri : region:Region.t -> Service.t -> [ `HTTP | `HTTPS ] -> Uri.t
