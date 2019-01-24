(** [Hashtbl.t] 's specialization. (To deal with [Qexp.t]) *)

open Bedrock

(** {2 Types} *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t
type configuration = (string, Qexp.t option) t

(**{2 Builder} *)

(** Build a [Table.t] from a [Qexp.t] *)
val from_qexp_gen :
     (Qexp.t -> Qexp.t list -> ('a * 'b) Result.t)
  -> Qexp.t
  -> ('a, 'b) t Result.t

(** Build a [Table.t] from a [Qexp.t] with k/v [Qexp.t]. *)
val from_qexp :
     (string -> Qexp.t list -> ('a * 'b) Result.t)
  -> Qexp.t
  -> ('a, 'b) t Result.t

(** Build a [Table.configuration] from a [Qexp.t]. *)
val configuration : Qexp.t -> configuration Result.t
