(** Describes a [Project] *)

open Paperwork

(** {2 Types} *)

(** Describes the [state] of a project. *)
type status =
  | Unceasing (** The project is always in progress; *)
  | Wip (** the project is in progress; *)
  | Done (** the project is done; *)
  | Paused (** the project is in pause; *)
  | Interrupted (** the project is stopped. *)

(** Describes a project *)
type t =
  { name : string
  ; title : string
  ; synopsis : string
  ; updated_at : Timetable.Day.t option
  ; repo : string option
  ; license : string option
  ; tools : Link.simple list
  ; links : Link.simple list
  ; releases : Link.dated list
  ; status : status
  ; tags : string list
  ; picto : string option
  ; indexed : bool
  ; content : Text.t option
  ; published : bool
  ; subprojects : t list
  }

(** {2 Api} *)

(** Produce a Project from a Qexp-reprsentation. *)
val from_qexp : Paperwork.Qexp.t -> t Bedrock.Validation.t

(** Render a status to a string. *)
val status_to_string : status -> string

(** {2 Utils} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
val to_json : t -> Json.t
val compare_date : t -> t -> int
