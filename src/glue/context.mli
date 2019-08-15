(** Build a context for page generation *)

(** A context for projects *)
module Projects : sig
  type t

  val update : t -> Shapes.Log.t -> t
  val init : Shapes.Update_table.t -> t
  val to_qexp : t -> Paperwork.Qexp.t
  val project_to_qexp : t -> string -> Paperwork.Qexp.t option
end
