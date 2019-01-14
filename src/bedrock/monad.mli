(** Monad

    This implementation is widely inspired by the 
    {{: http://hackage.haskell.org/} Haskell}'s implementation
*)

(** Build a new Monad's module using [REQUIREMENT_JOIN]. *)
module Make_with_join (M : Sigs.Monad.REQUIREMENT_JOIN) :
  Sigs.Monad.API with type 'a t = 'a M.t

(** Build a new Monad's module using [REQUIREMENT_BIND]. *)
module Make_with_bind (M : Sigs.Monad.REQUIREMENT_BIND) :
  Sigs.Monad.API with type 'a t = 'a M.t
