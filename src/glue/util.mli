(** Util in Glue context *)

open Bedrock
open Paperwork

(** {2 Helpers} *)

val now : unit -> Unix.tm
val current_year : unit -> int

(** {2 Timetable helpers} *)

val moment_of : Unix.tm -> Timetable.Moment.t Result.t

val moment_with_sec_of
  :  Unix.tm
  -> (Timetable.Moment.t * int) Result.t

val moment : unit -> Timetable.Moment.t Result.t
val moment_with_sec : unit -> (Timetable.Moment.t * int) Result.t
val hour_of : Unix.tm -> Timetable.Hour.t Result.t
val hour : unit -> Timetable.Hour.t Result.t
val day_of : Unix.tm -> Timetable.Day.t Result.t
val day : unit -> Timetable.Day.t Result.t
val month_of : Unix.tm -> Timetable.Month.t Result.t
val month : unit -> Timetable.Month.t Result.t
val year_of : Unix.tm -> Timetable.Year.t Result.t
val year : unit -> Timetable.Year.t Result.t
