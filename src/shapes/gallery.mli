open Bedrock
open Paperwork

type kind =
  | Photography
  | Illustration
  | Painting

type t =
  { name : string
  ; permalink : string
  ; content : Text.t
  ; description : string
  ; updated_at : Timetable.Day.t
  ; tags : string list
  ; kind : kind
  ; pictures : Picture.t list
  }

val new_gallery
  :  string
  -> string
  -> Text.t
  -> string
  -> Timetable.Day.t
  -> string list
  -> kind
  -> Picture.t list
  -> t

val kind_to_string : kind -> string
val kind_from_string : string -> kind Validation.t
val to_qexp : t -> Qexp.t
val from_qexp : Qexp.t -> t Validation.t
val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
