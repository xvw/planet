(** Interactive tags in planet *)

type t = string

type content =
  { title : string
  ; section : string
  ; id : string
  ; date : Paperwork.Timetable.Day.t
  ; tags : t list
  }

type bucket =
  { tags : t list
  ; contents : content list
  }

val to_qexp : bucket -> Paperwork.Qexp.t
val to_json : bucket -> Paperwork.Json.t
val sort : bucket -> bucket

val add :
     bucket
  -> string
  -> string
  -> string
  -> Paperwork.Timetable.Day.t
  -> t list
  -> bucket
