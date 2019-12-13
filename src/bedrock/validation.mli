(** Specialization over the type [('a, 'b) result] fixed using [Error.t list] as
    ['b]. *)

type 'a t = ('a, Error.t list) result
(** type for a validation. *)

val pop : (Error.t list -> 'a) -> 'a t -> 'a
(** Extract value of a result *)

val is_valid : 'a t -> bool

(** {2 Promotion's function} *)

val from_result : 'a Result.t -> 'a t
(** Promote ['a Result.t] to ['a Validation.t] *)

val from_option : Error.t -> 'a Option.t -> 'a t
(** Promote ['a Option.t] to ['a Validation.t] *)

(** {2 Functor instance} *)

module Functor : Sigs.Functor.API with type 'a t = 'a t

(** {2 Monad instance} *)

module Monad : sig
  include Sigs.Monad.API with type 'a t = 'a t
  include Sigs.TRAVERSABLE with type 'a t := 'a t
end

(** {2 Applicative instance} *)

module Applicative : sig
  include Sigs.Applicative.API with type 'a t = 'a t
  include Sigs.TRAVERSABLE with type 'a t := 'a t
end

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
