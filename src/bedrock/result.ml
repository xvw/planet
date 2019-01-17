type 'a t = ('a, Error.t) result
type 'a st = 'a t

module Functor = Functor.Make (struct
  type 'a t = 'a st

  let pure x = Ok x
  let map f = function Error x -> Error x | Ok x -> Ok (f x)
end)

module Monad = Monad.Make_with_bind (struct
  type 'a t = 'a st

  let return x = Ok x
  let bind f = function Error x -> Error x | Ok x -> f x
end)

module Applicative = Applicative.Make_from_monad (Monad)

module Infix = struct
  include Functor.Infix
  include Monad.Infix
  include Applicative.Infix
end

(** Instance inclusion *)

include Functor.Api
include Monad.Api
include Applicative.Api
include Infix
