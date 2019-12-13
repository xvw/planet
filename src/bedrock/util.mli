(** {2 Tools to work with functions} *)

val id : 'a -> 'a
(** Identity function, [id x] returns [x]. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** Reverse the order of arguments for 2-arity function. *)

val const : 'a -> 'b -> 'a
(** Produce a function that returns its first argument. [const a b] returns
    always [a]. *)

val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Function composition : [compose f g x] is equal to [g (f x)] *)

(** {2 Operators} *)

val ( $ ) : ('a -> 'b) -> 'a -> 'b
(** [f $ x] is equal to [f x] with low-pred *)

val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Alias for [compose] *)

val ( <% ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** [f <% g x] is equal to [f (g x)]. (Mathematical composition) *)

val ( % ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Alias for [<%]. *)

(** {2 Misc} *)

val md5 : string -> string
(** Produce the md5-hash of a string. *)

val bound : 'a -> 'a -> 'a -> 'a
(** [bound x min max] bounds [x] between [min] and [max]. *)

val try_until : ('a -> bool) -> (unit -> 'a) -> 'a
(** Perform a function until the result is valid. *)
