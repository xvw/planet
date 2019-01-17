(** Module to deal with files. *)

open Bedrock

(** {2 Types and aliases} *)

type name = string
type line = string
type chmod = int
type extension = string

(** {2 Information} *)

(** Returns [true] if a file exists, otherwise returns [false]. *)
val exists : name -> bool

(** Returns [Ok true] or [Ok false] if a file is (or not) a directory; 
    returns an error if the file does not exists.
 *)
val is_directory : name -> bool Result.t

(** {2 Read} *)

(** Read file into a stream. *)
val to_stream : name -> char Stream.t Result.t

(** Read file into a bytes squences. *)
val to_bytes : name -> bytes Result.t

(** Read file into a string. *)
val to_string : name -> string Result.t

(** Read file into chars list. *)
val chars : name -> char list Result.t

(** Read file into lines list. *)
val lines : name -> string list Result.t

(** {2 Write} *)

(** Write file. *)
val write :
     ?flags:open_flag list
  -> ?binary:bool
  -> ?append:bool
  -> ?chmod:chmod
  -> name
  -> string
  -> unit Result.t

(** Create a file. *)
val create : ?chmod:chmod -> name -> string -> unit Result.t

(** Append [content] to a file. *)
val append : ?chmod:chmod -> name -> string -> unit Result.t

(** Overwrite a file. *)
val overwrite : ?chmod:chmod -> name -> string -> unit Result.t

(** Delete a file. *)
val delete : name -> unit Result.t
