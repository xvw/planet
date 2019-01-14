(** The module exposes API and requirement. *)

module Base_Infix : sig
  (** The parametrized type. *)
  type 'a t

  (** Replace all locations in the input with the same value. *)
  val ( <$ ) : 'a -> 'b t -> 'a t

  (** Flipped version of [ <$ ]. *)
  val ( $> ) : 'a t -> 'b -> 'b t

  (** Infix version of [map]. *)
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  (** Flipped version of [<$>]. *)
  val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
end

module Functor : sig
  (** Describe the requirement to produce a [Functor] (as a Module) *)
  module type REQUIREMENT = sig
    (** The parametrized type. *)
    type 'a t

    (** Wrapp value into a Functor. *)
    val pure : 'a -> 'a t

    (** Mapping over ['a t]. *)
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  (** Describes a complete interface for functor. *)
  module type API = sig
    include REQUIREMENT

    (** Alias of [map]. *)
    val lift : ('a -> 'b) -> 'a t -> 'b t

    module Infix : module type of Base_Infix with type 'a t := 'a t
    include module type of Infix
  end
end

module Applicative : sig
  (** Describe the requirement to produce an [Applicative Functor] 
      (as a Module) 
  *)
  module type REQUIREMENT = sig
    (** The parametrized type. *)
    type 'a t

    (** Wrapp value into a Functor. *)
    val pure : 'a -> 'a t

    (** Sequential application *)
    val ap : ('a -> 'b) t -> 'a t -> 'b t
  end

  (** Describes a complete interface for applicative functor. *)
  module type API = sig
    include REQUIREMENT

    (** Mapping over ['a t]. *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Lift a unary function to actions. *)
    val lift : ('a -> 'b) -> 'a t -> 'b t

    (** Lift a binary function to actions. *)
    val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    (** Lift a ternary function to actions. *)
    val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

    (** Lift a quadratic function to actions. *)
    val lift4 :
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

    module Infix : sig
      include module type of Base_Infix with type 'a t := 'a t

      (** Infix version of [ap].*)
      val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

      (** Sequence actions, discarding the value of the first argument. *)
      val ( *> ) : 'a t -> 'b t -> 'b t

      (** Sequence actions, discarding the value of the second argument. *)
      val ( <* ) : 'a t -> 'b t -> 'a t

      (** Flipped version of [<*>]. *)
      val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
    end

    include module type of Infix
  end
end
