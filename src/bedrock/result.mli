(** Specialization over the type [('a, 'b) result] fixed using 
    [Error.t] as ['b].
 *)

(** type for a result. *)
type 'a t = ('a, Error.t) result

(** Extract value of a result *)
val pop : (Error.t -> 'a) -> 'a t -> 'a

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

(** Instance inclusion *)

include module type of Functor.Api
include module type of Monad.Api
include module type of Applicative.Api
include module type of Infix
include module type of Infix
