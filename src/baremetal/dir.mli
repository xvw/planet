(** Module related to [Directory] *)

open Bedrock

(** {2 Types} *)

type name = string
type children = File.name list

(** {2 Functions} *)

val exists : name -> bool
(** Check if a directory exists. *)

val children : ?filter:(File.name -> bool) -> name -> children Result.t
(** Get children of a directory. *)

val current : unit -> name
(** Get the working directory. *)

(** {2 Modify file-system} *)

val make : ?chmod:File.chmod -> name -> unit Result.t
(** Create new directory. *)

val delete : name -> unit Result.t
(** Remove a directory. *)
