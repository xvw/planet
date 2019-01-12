(** A functor is an uniform action over a parametrized type. 
    Generalizing the [map] function over a ['a t].

    This implementation is widely inspired by the 
    {{: http://hackage.haskell.org/} Haskell}'s implementation
*)

(** {2 Requirement} *)

(** Describe the requirement to produce a [Functor] (as a Module) *)
module type REQUIREMENT = sig
  (** The parametrized type. *)
  type 'a t

  (** Wrapp value into a Functor. *)
  val pure : 'a -> 'a t

  (** Mapping over ['a t]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** {2 Complete interface} *)

(** Describes a complete interface for functor. *)
module type API = sig
  include REQUIREMENT

  module Infix : sig
    (** Replace all locations in the input with the same value. *)
    val ( <$ ) : 'a -> 'b t -> 'a t

    (** Flipped version of [ <$ ]. *)
    val ( $> ) : 'a t -> 'b -> 'b t

    (** Infix version of [map]. *)
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

    (** Flipped version of [<$>]. *)
    val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
  end

  include module type of Infix
end

(** {2 Producing Functors} *)

(** Build a new Functor's module using [REQUIREMENT]. *)
module Make (F : REQUIREMENT) : API
