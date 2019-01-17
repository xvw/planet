type 'a t = 'a list

module Functor = Functor.Make (struct
  type 'a t = 'a list

  let pure x = [x]
  let map f x = Stdlib.List.map f x
end)

module Monad = Monad.Make_with_join (struct
  type 'a t = 'a list

  let return x = [x]
  let map = Stdlib.List.map
  let join = Stdlib.List.concat
end)

module Applicative = Applicative.Make_from_monad (Monad)
include Stdlib.List

module Infix = struct
  include Functor.Infix
  include Monad.Infix
  include Applicative.Infix
end

include Infix
