(** This module describes a structure intermediate between a functor 
    and a monad.

    This implementation is widely inspired by the 
    {{: http://hackage.haskell.org/} Haskell}'s implementation
*)

(** Build a new Applicative Functor's module using [REQUIREMENT]. *)
module Make (F : Sigs.Applicative.REQUIREMENT) :
  Sigs.Applicative.API with type 'a t = 'a F.t

(** Build a new Applicative Functor's module using a [Monad]. *)
module Make_from_monad (M : Sigs.Monad.REQUIREMENT_BIND) :
  Sigs.Applicative.API with type 'a t = 'a M.t
