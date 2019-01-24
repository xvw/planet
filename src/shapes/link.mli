(** Describe links *)

type uri = string
type name = string

(** Describe a simple link. *)
type simple = name * uri

(** Dated link. *)
type dated = name * Paperwork.Timetable.Day.t * uri
