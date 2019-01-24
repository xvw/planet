(** Describes a [Project] *)

(** {2 Types} *)

(** Describes the [state] of a project. *)
type status =
  | Unceasing  (** The project is always in progress; *)
  | Wip  (** the project is in progress; *)
  | Done  (** the project is done; *)
  | Paused  (** the project is in pause; *)
  | Interrupted  (** the project is stopped. *)

(** Describes a project *)
type t =
  { name : string
  ; title : string
  ; synopsis : string
  ; repo : string option
  ; license : string option
  ; tools : Link.simple list
  ; links : Link.simple list
  ; releases : Link.dated list
  ; status : status
  ; tags : string list
  ; picto : string option
  ; indexed : bool
  ; content : Text.t option }

(** {2 Api} *)
(* (\** Produce a Project from a Qexp-reprsentation. *\)
 * val from_qexp : Paperwork.Qexp.t -> t Bedrock.Validation.t *)
