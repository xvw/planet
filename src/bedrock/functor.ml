open Util

module Make (F : Sigs.Functor.REQUIREMENT) :
  Sigs.Functor.API with type 'a t = 'a F.t = struct
  include F

  let lift = map

  module Infix = struct
    let ( <$> ) = map
    let ( <&> ) x f = map f x
    let ( <$ ) x tx = (map % const) x tx
    let ( $> ) tx x = x <$ tx
  end

  include Infix
end
