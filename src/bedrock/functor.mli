(** A functor is an uniform action over a parametrized type. 
    Generalizing the [map] function over a ['a t].

    This implementation is widely inspired by the 
    {{: http://hackage.haskell.org/} Haskell}'s implementation
*)

(** Build a new Functor's module using [REQUIREMENT]. *)
module Make (F : Sigs.Functor.REQUIREMENT) :
  Sigs.Functor.API with type 'a t = 'a F.t
