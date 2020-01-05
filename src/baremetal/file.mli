(** Module to deal with files. *)

open Bedrock

(** {2 Types and aliases} *)

type name = string
type line = string
type chmod = int
type extension = string
type old_name = string
type new_name = string

(** {2 Information} *)

(** Returns [true] if a file exists, otherwise returns [false]. *)
val exists : name -> bool

(** Returns [Ok true] or [Ok false] if a file is (or not) a directory; returns
    an error if the file does not exists. *)
val is_directory : name -> bool Result.t

(** {2 Read} *)

(** Open reading. *)
val in_channel : name -> in_channel Result.t

(** Close read channel. *)
val close_in : in_channel -> unit

(** Read file *)
val read : (in_channel -> 'a Result.t) -> name -> 'a Result.t

(** Read file into a stream. *)
val to_stream : (name -> char Stream.t -> 'a Result.t) -> name -> 'a Result.t

(** Read file into a bytes squences. *)
val to_bytes : name -> bytes Result.t

(** Read file into a string. *)
val to_string : name -> string Result.t

(** Read file into chars list. *)
val chars : name -> char list Result.t

(** Read file into lines list. *)
val lines : name -> string list Result.t

(** {2 Write} *)

(** Open file for writting. *)
val out_channel
  :  ?flags:open_flag list
  -> ?binary:bool
  -> ?append:bool
  -> ?chmod:chmod
  -> ?overwrite:bool
  -> name
  -> out_channel Result.t

(** Close writting channel. *)
val close_out : out_channel -> unit

(** Write file. *)
val write
  :  ?flags:open_flag list
  -> ?binary:bool
  -> ?append:bool
  -> ?chmod:chmod
  -> ?overwrite:bool
  -> (out_channel -> 'a Result.t)
  -> name
  -> 'a Result.t

(** Create a file. *)
val create : ?binary:bool -> ?chmod:chmod -> name -> string -> unit Result.t

(** Append [content] to a file. *)
val append
  :  ?binary:bool
  -> ?create:bool
  -> ?chmod:chmod
  -> name
  -> string
  -> unit Result.t

(** Overwrite a file. *)
val overwrite
  :  ?binary:bool
  -> ?create:bool
  -> ?chmod:chmod
  -> name
  -> string
  -> unit Result.t

(** Create a file if not exists *)
val touch : ?binary:bool -> ?chmod:chmod -> name -> unit Result.t

(** Delete a file. *)
val delete : name -> unit Result.t

(** Rename a file *)
val rename : old_name -> new_name -> unit Result.t
