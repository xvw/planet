(** Pre-implemented functors *)

(** Functor implementation for ['a list] *)
module List : Sigs.Functor.API with type 'a t = 'a list

(** Functor implementation for ['a option] *)
module Option : Sigs.Functor.API with type 'a t = 'a option

(** Functor implementation for ['a array] *)
module Array : Sigs.Functor.API with type 'a t = 'a array

(** Functor implementation for ['a Result.t] *)
module Result : Sigs.Functor.API with type 'a t = 'a Result.t
