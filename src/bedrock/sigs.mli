(** The module exposes API and requirement. *)

module Base_Infix : sig
  type 'a t
  (** The parametrized type. *)

  val ( <$ ) : 'a -> 'b t -> 'a t
  (** Replace all locations in the input with the same value. *)

  val ( $> ) : 'a t -> 'b -> 'b t
  (** Flipped version of [ <$ ]. *)

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  (** Infix version of [map]. *)

  val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
  (** Flipped version of [<$>]. *)
end

module Base_Lift : sig
  type 'a t
  (** The parametrized type. *)

  val lift : ('a -> 'b) -> 'a t -> 'b t
  (** Lift a unary function to actions. *)

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Lift a binary function to actions. *)

  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Lift a ternary function to actions. *)

  val lift4 :
    ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  (** Lift a quadratic function to actions. *)
end

module Functor : sig
  (** Describe the requirement to produce a [Functor] (as a Module) *)
  module type REQUIREMENT = sig
    type 'a t
    (** The parametrized type. *)

    val pure : 'a -> 'a t
    (** Wrap value into a Functor. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** Mapping over ['a t]. *)
  end

  (** Describes a complete interface for functor. *)
  module type API = sig
    type 'a t

    module Api : sig
      include REQUIREMENT with type 'a t := 'a t

      val lift : ('a -> 'b) -> 'a t -> 'b t
      (** Alias of [map]. *)
    end

    include module type of Api
    module Infix : module type of Base_Infix with type 'a t := 'a t
    include module type of Infix
  end
end

module Applicative : sig
  (** Describe the requirement to produce an [Applicative Functor] (as a Module) *)
  module type REQUIREMENT = sig
    type 'a t
    (** The parametrized type. *)

    val pure : 'a -> 'a t
    (** Wrap value into an Applicative. *)

    val ap : ('a -> 'b) t -> 'a t -> 'b t
    (** Sequential application *)
  end

  (** Describes a complete interface for applicative functor. *)
  module type API = sig
    type 'a t

    module Api : sig
      include REQUIREMENT with type 'a t := 'a t

      val map : ('a -> 'b) -> 'a t -> 'b t
      (** Mapping over ['a t]. *)

      include module type of Base_Lift with type 'a t := 'a t
    end

    include module type of Api

    module Infix : sig
      include module type of Base_Infix with type 'a t := 'a t

      val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
      (** Infix version of [ap].*)

      val ( *> ) : 'a t -> 'b t -> 'b t
      (** Sequence actions, discarding the value of the first argument. *)

      val ( <* ) : 'a t -> 'b t -> 'a t
      (** Sequence actions, discarding the value of the second argument. *)

      val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
      (** Flipped version of [<*>]. *)
    end

    include module type of Infix

    module Syntax : sig
      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
      val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    end
  end
end

module Monad : sig
  module type REQUIREMENT_BIND = sig
    type 'a t
    (** The parametrized type. *)

    val return : 'a -> 'a t
    (** Wrap value into an Applicative. *)

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** Sequentially compose two actions, passing any value produced by the
        first as an argument to the second. *)
  end

  module type REQUIREMENT_JOIN = sig
    type 'a t
    (** The parametrized type. *)

    val return : 'a -> 'a t
    (** Wrap value into an Applicative. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** Mapping over ['a t]. *)

    val join : 'a t t -> 'a t
    (** The join function is the conventional monad join operator. It is used to
        remove one level of monadic structure, projecting its bound argument
        into the outer level. *)
  end

  module type API = sig
    type 'a t

    module Api : sig
      include REQUIREMENT_JOIN with type 'a t := 'a t
      include REQUIREMENT_BIND with type 'a t := 'a t
      include module type of Base_Lift with type 'a t := 'a t

      val void : 'a t -> unit t
      (** void value discards or ignores the result of evaluation, such as the
          return value of an IO action. *)
    end

    include module type of Api

    module Infix : sig
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      (** Flipped infix version of [bind]. *)

      val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
      (** Flipped infix version of [map]. *)

      val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
      (** Right-to-left Kleisli composition of monads. [(>=>)], with the
          arguments flipped *)

      val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
      (** Left-to-right Kleisli composition of monads. *)

      val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t
      (** Flipped version of [>>=] *)

      val ( >> ) : 'a t -> 'b t -> 'b t
      (** Sequentially compose two actions, discarding any value produced by the
          first, like sequencing operators (such as the semicolon) in imperative
          languages. *)
    end

    include module type of Infix

    module Syntax : sig
      val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    end

    include module type of Syntax
  end
end

module type TRAVERSABLE = sig
  type 'a t
  (** The parametrized type. *)

  val traverse : ('a -> 'b t) -> 'a list -> 'b list t
  val sequence : 'a t list -> 'a list t
end
