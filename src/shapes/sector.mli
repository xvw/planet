(** Describe a [Sector] *)

open Bedrock
open Paperwork

(** {2 Type} *)

type t =
  { name : string
  ; desc : string
  ; color : Color.t
  }

(** {2 Functions} *)

val make : string -> string -> Color.t -> t
val to_qexp : t -> Qexp.t
val from_qexp : Qexp.t -> t Validation.t

(** {2 Utils} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
val to_json : t -> Json.t
