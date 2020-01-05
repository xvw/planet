(** {2 Tools to work with functions} *)

(** Identity function, [id x] returns [x]. *)
val id : 'a -> 'a

(** Reverse the order of arguments for 2-arity function. *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** Produce a function that returns its first argument. [const a b] returns
    always [a]. *)
val const : 'a -> 'b -> 'a

(** Function composition : [compose f g x] is equal to [g (f x)] *)
val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** {2 Operators} *)

(** [f $ x] is equal to [f x] with low-pred *)
val ( $ ) : ('a -> 'b) -> 'a -> 'b

(** Alias for [compose] *)
val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** [f <% g x] is equal to [f (g x)]. (Mathematical composition) *)
val ( <% ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** Alias for [<%]. *)
val ( % ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** {2 Misc} *)

(** Produce the md5-hash of a string. *)
val md5 : string -> string

(** [bound x min max] bounds [x] between [min] and [max]. *)
val bound : 'a -> 'a -> 'a -> 'a

(** Perform a function until the result is valid. *)
val try_until : ('a -> bool) -> (unit -> 'a) -> 'a
