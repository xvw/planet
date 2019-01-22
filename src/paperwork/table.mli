(** [Hashtbl.t] 's specialization. (To deal with [Qexp.t]) *)

open Bedrock

(** {2 Types} *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

(**{2 Builder} *)

(** Build a [Table.t] from a [Qexp.t] *)
val from_qexp :
     (Qexp.t -> Qexp.t list -> ('a * 'b) Result.t)
  -> Qexp.t
  -> ('a, 'b) t Result.t
