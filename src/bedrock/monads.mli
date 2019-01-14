(** Pre-implemented monads *)

(** Monad implementation for ['a list] *)
module List : Sigs.Monad.API with type 'a t = 'a list

(** Monad implementation for ['a option] *)
module Option : Sigs.Monad.API with type 'a t = 'a option

(** Monad implementation for ['a array] *)
module Array : Sigs.Monad.API with type 'a t = 'a array

(** Monad implementation for ['a Result.t] *)
module Result : Sigs.Monad.API with type 'a t = 'a Result.t
