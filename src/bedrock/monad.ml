open Util

module type REQ = sig
  include Sigs.Monad.REQUIREMENT_BIND
  include Sigs.Monad.REQUIREMENT_JOIN with type 'a t := 'a t
end

module Join (M : Sigs.Monad.REQUIREMENT_JOIN) :
  REQ with type 'a t = 'a M.t = struct
  include M

  let bind f m = join (map f m)
end

module Bind (M : Sigs.Monad.REQUIREMENT_BIND) :
  REQ with type 'a t = 'a M.t = struct
  include M

  let join m = bind id m
  let map f m = bind (return % f) m
end

module WithReq (M : REQ) : Sigs.Monad.API with type 'a t = 'a M.t =
struct
  module Api = struct
    include M

    let ( >>= ) x f = bind f x
    let lift = map
    let lift2 f a b = a >>= fun x -> b >>= fun y -> return (f x y)

    let lift3 f a b c =
      a >>= fun x -> b >>= fun y -> c >>= fun z -> return (f x y z)
    ;;

    let lift4 f a b c d =
      a
      >>= fun w ->
      b >>= fun x -> c >>= fun y -> d >>= fun z -> return (f w x y z)
    ;;

    let void _ = return ()
  end

  include Api

  module Infix = struct
    let ( >>= ) x f = M.bind f x
    let ( >|= ) x f = M.map f x
    let ( >> ) m n = m >>= fun _ -> n
    let ( <=< ) f g x = g x >>= f
    let ( >=> ) f g = flip ( <=< ) f g
    let ( =<< ) = M.bind
  end

  include Infix

  module Syntax = struct
    let ( let* ) x f = bind f x
  end

  include Syntax
end

module Make_with_join (M : Sigs.Monad.REQUIREMENT_JOIN) :
  Sigs.Monad.API with type 'a t = 'a M.t = struct
  include WithReq (Join (M))
end

module Make_with_bind (M : Sigs.Monad.REQUIREMENT_BIND) :
  Sigs.Monad.API with type 'a t = 'a M.t = struct
  include WithReq (Bind (M))
end
