open Util

module Make (A : Sigs.Applicative.REQUIREMENT) :
  Sigs.Applicative.API with type 'a t = 'a A.t = struct
  module F = Functor.Make (struct
    type 'a t = 'a A.t

    let pure = A.pure
    let map f x = A.ap (pure f) x
  end)

  module Api = struct
    include A

    let map = F.map
    let lift = F.lift
    let lift2 f a = ap (lift f a)
    let lift3 f a b c = ap (lift2 f a b) c
    let lift4 f a b c d = ap (lift3 f a b c) d
  end

  include Api

  module Infix = struct
    include F.Infix

    let ( <*> ) = ap
    let ( <**> ) x f = f <*> x
    let ( <* ) a b = lift2 const a b
    let ( *> ) a b = id <$ a <*> b
  end

  include Infix

  module Syntax = struct
    let ( let+ ) x f = map f x
    let ( and+ ) xa ya = pure (fun x y -> x, y) <*> xa <*> ya
  end
end

module From_monad (M : Sigs.Monad.REQUIREMENT_BIND) :
  Sigs.Applicative.REQUIREMENT with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t

  let pure = M.return
  let ap fs xs = M.bind (fun f -> M.bind (fun x -> pure (f x)) xs) fs
end

module Make_from_monad (M : Sigs.Monad.REQUIREMENT_BIND) :
  Sigs.Applicative.API with type 'a t = 'a M.t = struct
  include Make (From_monad (M))
end
