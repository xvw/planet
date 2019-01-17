(** Module to deal with files. *)

open Bedrock

(** {2 Types and aliases} *)

type name = string
type line = string
type chmod = int
type extension = string

(** {2 Read} *)

(** Read file into a stream. *)
val to_stream : name -> char Stream.t Result.t

(** Read file into a bytes squences. *)
val to_bytes : name -> bytes Result.t

(** Read file into a string. *)
val to_string : name -> string Result.t

(** Read file into chars list *)
val chars : name -> char list Result.t

(** Read file into lines list *)
val lines : name -> string list Result.t
