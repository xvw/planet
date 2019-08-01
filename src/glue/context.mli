(** Build a context for page generation *)

type project_context =
  { project_context_sectors : (string, int) Hashtbl.t
  ; project_context_counter : int
  ; project_context_streak : int
  }

type t =
  { sectors : (string, int) Hashtbl.t
  ; projects : (string, project_context) Hashtbl.t
  ; streak : int
  ; counter : int
  ; update_table : Shapes.Update_table.t
  }
