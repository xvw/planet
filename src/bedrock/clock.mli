(** Clock (or effectful-counter) *)

type 'a t
(** Type for a clock. *)

val make : decr:('a -> 'a) -> incr:('a -> 'a) -> 'a -> 'a t
(** Make a new clock. *)

val current : 'a t -> 'a
(** Get the current value of a clock *)

val next : 'a t -> 'a
(** Get (and set) the next value of a clock. *)

val previous : 'a t -> 'a
(** Get (and set) the previous value of a clock. *)

val reset : 'a t -> unit
(** Reset a clock. *)

(** {2 Presaved clocks} *)

val int : unit -> int t
(** Clock for the integers. *)
