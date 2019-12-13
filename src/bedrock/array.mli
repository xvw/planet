(** Extension of [Stdlib.Array] *)

include module type of Stdlib.Array
(** {2 Stdlib} *)

module Functor : Sigs.Functor.API with type 'a t = 'a array
(** {2 Functor instance} *)

module Monad : Sigs.Monad.API with type 'a t = 'a array
(** {2 Monad instance} *)

module Applicative : Sigs.Applicative.API with type 'a t = 'a array
(** {2 Applicative instance} *)

(** {2 Infix operators} *)

module Infix : sig
  include module type of Functor.Infix
  include module type of Monad.Infix
  include module type of Applicative.Infix
end

(** {2 Syntax} *)

module Syntax : sig
  include module type of Monad.Syntax
  include module type of Applicative.Syntax
end

(** Instance inclusion *)

include module type of Functor.Api
include module type of Monad.Api
include module type of Applicative.Api
include module type of Infix
include module type of Syntax
