(** Build a context for page generation *)

(** A context for projects *)
module Projects : sig
  type context =
    { name : string
    ; start_date : Paperwork.Timetable.Day.t option
    ; last_update : Paperwork.Timetable.Day.t option
    ; logs_counter : int
    ; minuts_counter : int
    ; sectors_counters : (string, int) Hashtbl.t
    }

  type t =
    { updates : Update_table.t
    ; projects : (string, context) Hashtbl.t
    }

  val update : t -> Log.t -> t
  val init : Update_table.t -> t
  val to_qexp : t -> Paperwork.Qexp.t
  val project_to_qexp : string -> context -> Paperwork.Qexp.t

  val project_from_qexp
    :  Paperwork.Qexp.t
    -> context Bedrock.Validation.t
end

type context =
  { start_date : Paperwork.Timetable.Day.t option
  ; last_update : Paperwork.Timetable.Day.t option
  ; logs_counter : int
  ; minuts_counter : int
  ; sectors_counters : (string, int) Hashtbl.t
  }

type t =
  { projects_data : Projects.t
  ; global_data : context
  }

val init : Update_table.t -> t
val update : t -> Log.t -> t
val context_to_qexp : context -> Paperwork.Qexp.t

val context_from_qexp
  :  Paperwork.Qexp.t
  -> context Bedrock.Validation.t
