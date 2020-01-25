(** Describe Pictures *)

open Bedrock
open Paperwork

type t =
  { name : string
  ; description : string
  ; date : Timetable.Day.t
  ; tools : string list
  ; tags : string list
  ; place : (string * string) option
  ; image : string
  ; thumbnail : string option
  }

val new_picture
  :  string
  -> string
  -> Timetable.Day.t
  -> string list
  -> string list
  -> (string * string) option
  -> string
  -> string option
  -> t

val to_qexp : t -> Qexp.t
val from_qexp : Qexp.t -> t Validation.t
val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
