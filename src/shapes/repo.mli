(** Handle heterogenous CVS Repo *)

type t

(** {2 Repo references} *)

val github : user:string -> string -> t
val gitlab : user:string -> string -> t

(** {2 Various data} *)

val repr : t -> string

(** {2 Links generators} *)

val base_url : t -> string
val https_reference : t -> string
val ssh_reference : t -> string
val bucktracker_url : t -> string
val releases_url : t -> string

(** {2 Serialization/Deserialization} *)

val to_qexp : t -> Paperwork.Qexp.t
val from_qexp : Paperwork.Qexp.t -> t Bedrock.Validation.t
