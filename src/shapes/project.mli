(** Describes a [Project] *)

open Paperwork

(** {2 Types} *)

(** Describes the [state] of a project. *)
type status =
  | Unceasing  (** The project is always in progress; *)
  | Wip  (** the project is in progress; *)
  | Done  (** the project is done; *)
  | Paused  (** the project is in pause; *)
  | Interrupted  (** the project is stopped. *)

type t =
  { name : string
  ; title : string
  ; synopsis : string
  ; repo : Repo.t option
  ; license : string option
  ; links : (string * Link.simple list) list
  ; releases : Link.dated list
  ; status : status
  ; tags : string list
  ; picto : string option
  ; indexed : bool
  ; content : Text.t option
  ; published : bool
  ; subprojects : t list
  }
(** Describes a project *)

(** {2 Api} *)

val from_qexp : Paperwork.Qexp.t -> t Bedrock.Validation.t
(** Produce a Project from a Qexp-reprsentation. *)

val to_qexp : t -> Paperwork.Qexp.t
(** Produce a [Qexp] from a [Project]. *)

val status_to_string : status -> string
(** Render a status to a string. *)

(** {2 Utils} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
val to_json : t -> Json.t
