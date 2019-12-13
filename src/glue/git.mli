(** Git helper *)

open Bedrock
open Baremetal

val stage : File.name list -> unit Result.t
(** Stages files *)

val commit : ?desc:string -> string -> unit Result.t
(** Commit changes *)
