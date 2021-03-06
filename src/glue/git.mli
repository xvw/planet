(** Git helper *)

open Bedrock
open Baremetal

(** Stages files *)
val stage : File.name list -> unit Result.t

(** Commit changes *)
val commit : ?desc:string -> string -> unit Result.t
