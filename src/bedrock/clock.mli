(** Clock (or effectful-counter) *)

(** Type for a clock. *)
type 'a t

(** Make a new clock. *)
val make : decr:('a -> 'a) -> incr:('a -> 'a) -> 'a -> 'a t

(** Get the current value of a clock *)
val current : 'a t -> 'a

(** Get (and set) the next value of a clock. *)
val next : 'a t -> 'a

(** Get (and set) the previous value of a clock. *)
val previous : 'a t -> 'a

(** Reset a clock. *)
val reset : 'a t -> unit

(** {2 Presaved clocks} *)

(** Clock for the integers. *)
val int : unit -> int t
