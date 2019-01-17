(** Extension of [Stdlib.Array] *)

(** Type for an array. *)
type 'a t = 'a array

(** {2 Functor instance} *)
module Functor : Sigs.Functor.API with type 'a t = 'a array

(** {2 Monad instance} *)
module Monad : Sigs.Monad.API with type 'a t = 'a array

(** {2 Applicative instance} *)
module Applicative : Sigs.Applicative.API with type 'a t = 'a array

(** {2 Stdlib} *)
include module type of Stdlib.Array

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
include module type of Infix
