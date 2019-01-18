(** Module related to [Directory] *)

open Bedrock

(** {2 Types} *)

type name = string
type children = File.name list

(** {2 Functions} *)

(** Check if a directory exists. *)
val exists : name -> bool

(** Get children of a directory. *)
val children :
  ?filter:(File.name -> bool) -> name -> children Result.t

(** Get the working directory. *)
val current : unit -> name

(** {2 Modify file-system} *)

(** Create new directory. *)
val make : ?chmod:File.chmod -> name -> unit Result.t
