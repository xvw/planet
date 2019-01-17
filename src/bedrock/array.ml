type 'a t = 'a array

module Functor = Functor.Make (struct
  type 'a t = 'a array

  let pure x = [|x|]
  let map f x = Stdlib.Array.map f x
end)

module Monad = Monad.Make_with_bind (struct
  type 'a t = 'a array

  let return x = [|x|]

  let bind f x =
    Stdlib.Array.fold_right
      (fun x acc -> Stdlib.Array.concat [f x; acc])
      x
      [||]
  ;;
end)

module Applicative = Applicative.Make_from_monad (Monad)
include Stdlib.Array

module Infix = struct
  include Functor.Infix
  include Monad.Infix
  include Applicative.Infix
end

include Functor.Api
include Monad.Api
include Applicative.Api
include Infix
