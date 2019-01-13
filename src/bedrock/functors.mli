(** Pre-implemented functors *)

(** Functor implementation for ['a list] *)
module List : Functor.API with type 'a t = 'a list

(** Functor implementation for ['a option] *)
module Option : Functor.API with type 'a t = 'a option

(** Functor implementation for ['a array] *)
module Array : Functor.API with type 'a t = 'a array

(** Functor implementation for ['a Result.t] *)
module Result : Functor.API with type 'a t = 'a Result.t
