(** Option module, to deal with value's absence *)

(** Type for option. *)
type 'a t = 'a option

(** {2 Functor instance} *)
module Functor : Sigs.Functor.API with type 'a t = 'a option

(** {2 Monad instance} *)
module Monad : Sigs.Monad.API with type 'a t = 'a option

(** {2 Applicative instance} *)
module Applicative : Sigs.Applicative.API with type 'a t = 'a option

(** {2 Infix operators} *)

module Infix : sig
  include module type of Functor.Infix
  include module type of Monad.Infix
  include module type of Applicative.Infix
end

include module type of Infix
