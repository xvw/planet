(** Extension of [Stdlib.List] *)

(** Type for a list. *)
type 'a t = 'a list

(** {2 Extension API} *)

val zip : 'a t -> 'b t -> ('a * 'b) t option
val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** {2 Functor instance} *)
module Functor : Sigs.Functor.API with type 'a t = 'a list

(** {2 Monad instance} *)
module Monad : sig
  include Sigs.Monad.API with type 'a t = 'a list

  (** Produce a List Traversable from a Monad *)
  module Traversable (M : Sigs.Monad.API) :
    Sigs.TRAVERSABLE with type 'a t = 'a M.t
end

(** {2 Applicative instance} *)
module Applicative : sig
  include Sigs.Applicative.API with type 'a t = 'a list

  (** Produce a List Traversable from an Applicative *)
  module Traversable (A : Sigs.Applicative.API) :
    Sigs.TRAVERSABLE with type 'a t = 'a A.t
end

(** {2 Stdlib} *)
include module type of Stdlib.List

(** {2 Infix operators} *)

module Infix : sig
  include module type of Functor.Infix
  include module type of Monad.Infix
  include module type of Applicative.Infix
end

(** Instance inclusion *)

include module type of Functor.Api
include module type of Monad.Api
include module type of Applicative.Api
include module type of Infix
