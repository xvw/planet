(** Module to build Unique identifiers *)

type t = string

(** {2 Requirement} *)

module type GENERATOR = sig
  val name : unit -> string
  val pid : unit -> int
  val time : unit -> float
end

(** {2 Global API} *)

val make : (module GENERATOR) -> unit -> t
(** Create an [UUID]. *)

(** {2 Functor} *)

module Generator (G : GENERATOR) : sig
  type t = string

  val make : unit -> t
  val to_string : t -> string
end
