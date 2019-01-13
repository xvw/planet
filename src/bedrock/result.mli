(** Specialization over the type [('a, 'b) result] fixed using 
    [Error.t] as ['b].
 *)

type 'a t = ('a, Error.t) result
