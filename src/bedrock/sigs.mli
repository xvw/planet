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

module Base_Lift : sig
  (** The parametrized type. *)
  type 'a t

  (** Lift a unary function to actions. *)
  val lift : ('a -> 'b) -> 'a t -> 'b t

  (** Lift a binary function to actions. *)
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  (** Lift a ternary function to actions. *)
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  (** Lift a quadratic function to actions. *)
  val lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
end

module Functor : sig
  (** Describe the requirement to produce a [Functor] (as a Module) *)
  module type REQUIREMENT = sig
    (** The parametrized type. *)
    type 'a t

    (** Wrap value into a Functor. *)
    val pure : 'a -> 'a t

    (** Mapping over ['a t]. *)
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  (** Describes a complete interface for functor. *)
  module type API = sig
    type 'a t

    module Api : sig
      include REQUIREMENT with type 'a t := 'a t

      (** Alias of [map]. *)
      val lift : ('a -> 'b) -> 'a t -> 'b t
    end

    include module type of Api
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

    (** Wrap value into an Applicative. *)
    val pure : 'a -> 'a t

    (** Sequential application *)
    val ap : ('a -> 'b) t -> 'a t -> 'b t
  end

  (** Describes a complete interface for applicative functor. *)
  module type API = sig
    type 'a t

    module Api : sig
      include REQUIREMENT with type 'a t := 'a t

      (** Mapping over ['a t]. *)
      val map : ('a -> 'b) -> 'a t -> 'b t

      include module type of Base_Lift with type 'a t := 'a t
    end

    include module type of Api

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

module Monad : sig
  module type REQUIREMENT_BIND = sig
    (** The parametrized type. *)
    type 'a t

    (** Wrap value into an Applicative. *)
    val return : 'a -> 'a t

    (** Sequentially compose two actions, passing any value produced by the
        first as an argument to the second.
    *)
    val bind : ('a -> 'b t) -> 'a t -> 'b t
  end

  module type REQUIREMENT_JOIN = sig
    (** The parametrized type. *)
    type 'a t

    (** Wrap value into an Applicative. *)
    val return : 'a -> 'a t

    (** Mapping over ['a t]. *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** The join function is the conventional monad join operator.
        It is used to remove one level of monadic structure, projecting its
        bound argument into the outer level.
    *)
    val join : 'a t t -> 'a t
  end

  module type API = sig
    type 'a t

    module Api : sig
      include REQUIREMENT_JOIN with type 'a t := 'a t
      include REQUIREMENT_BIND with type 'a t := 'a t
      include module type of Base_Lift with type 'a t := 'a t

      (** void value discards or ignores the result of evaluation, such as the
        return value of an IO action. 
    *)
      val void : 'a t -> unit t
    end

    include module type of Api

    module Infix : sig
      (** Flipped infix version of [bind]. *)
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

      (** Flipped infix version of [map]. *)
      val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

      (** Right-to-left Kleisli composition of monads. [(>=>)], 
          with the arguments flipped 
      *)
      val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t

      (** Left-to-right Kleisli composition of monads. *)
      val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

      (** Flipped version of [>>=] *)
      val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t

      (** Sequentially compose two actions, discarding any value produced 
          by the first, like sequencing operators (such as the semicolon) 
          in imperative languages. 
      *)
      val ( >> ) : 'a t -> 'b t -> 'b t
    end

    include module type of Infix
  end
end

module type TRAVERSABLE = sig
  (** The parametrized type. *)
  type 'a t

  val traverse : ('a -> 'b t) -> 'a list -> 'b list t
  val sequence : 'a t list -> 'a list t
end
