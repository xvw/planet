type 'a t = 'a option

module Functor = Functor.Make (struct
  type 'a t = 'a option

  let pure x = Some x
  let map f = function None -> None | Some x -> Some (f x)
end)

module Monad = Monad.Make_with_bind (struct
  type 'a t = 'a option

  let return x = Some x
  let bind f = function None -> None | Some x -> f x
end)

module Applicative = Applicative.Make_from_monad (Monad)

module Infix = struct
  include Functor.Infix
  include Monad.Infix
  include Applicative.Infix
end

include Functor.Api
include Monad.Api
include Applicative.Api
include Infix
