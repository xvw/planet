(** Interactive tags in planet *)

type t = string

type content =
  { title : string
  ; section : string
  ; id : string
  ; date : Paperwork.Timetable.Day.t
  ; description : string
  ; tags : t list
  }

type bucket =
  { all_tags : t list
  ; contents : content list
  }

val new_bucket : unit -> bucket
val to_qexp : bucket -> Paperwork.Qexp.t
val to_json : bucket -> Paperwork.Json.t
val sort : bucket -> bucket

val add
  :  bucket
  -> string
  -> string
  -> string
  -> Paperwork.Timetable.Day.t
  -> string
  -> t list
  -> bucket
