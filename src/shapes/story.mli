(** Describes a [Story] *)

open Paperwork

(** {2 Types} *)

(** Kind of story *)
type kind =
  | Long
  | Short

type t =
  { permaname : string
  ; title : string
  ; synopsis : string
  ; links : (string * Link.simple list) list
  ; content : Text.t
  ; published : bool
  ; related_project : string option
  ; category : string
  ; tags : string list
  ; date : Timetable.Day.t
  ; kind : kind
  }
(** A Story *)

(** {2 Api} *)

val from_qexp : Paperwork.Qexp.t -> t Bedrock.Validation.t
(** Produce a Story from a Qexp-reprsentation. *)

val to_qexp : t -> Paperwork.Qexp.t
(** Produce a [Qexp] from a [Story]. *)

val kind_to_string : kind -> string
(** Render a kind to a string. *)

(** {2 Utils} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
val to_json : t -> Json.t
