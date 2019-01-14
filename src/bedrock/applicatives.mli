(** Pre-implemented applicative functors *)

(** Functor implementation for ['a list] *)
module List : Sigs.Applicative.API with type 'a t = 'a list

(** Functor implementation for ['a option] *)
module Option : Sigs.Applicative.API with type 'a t = 'a option

(** Functor implementation for ['a array] *)
module Array : Sigs.Applicative.API with type 'a t = 'a array

(** Functor implementation for ['a Result.t] *)
module Result : Sigs.Applicative.API with type 'a t = 'a Result.t
