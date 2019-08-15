(** Build a context for page generation *)

(** A context for projects *)
module Projects : sig
  type context =
    { name : string
    ; start_date : Paperwork.Timetable.Day.t option
    ; logs_counter : int
    ; minuts_counter : int
    ; sectors_counters : (string, int) Hashtbl.t
    }

  type t =
    { updates : Shapes.Update_table.t
    ; projects : (string, context) Hashtbl.t
    }

  val update : t -> Shapes.Log.t -> t
  val init : Shapes.Update_table.t -> t
  val to_qexp : t -> Paperwork.Qexp.t
  val project_to_qexp : string -> context -> Paperwork.Qexp.t

  (* val project_from_qexp : Paperwork.Qexp.t -> context
   * val project_from_string : string -> context *)
end
