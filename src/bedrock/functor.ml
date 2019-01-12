open Util

module type REQUIREMENT = sig
  type 'a t

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type API = sig
  include REQUIREMENT

  module Infix : sig
    val ( <$ ) : 'a -> 'b t -> 'a t
    val ( $> ) : 'a t -> 'b -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
  end

  include module type of Infix
end

module Make (F : REQUIREMENT) : API with type 'a t = 'a F.t = struct
  include F

  module Infix = struct
    let ( <$> ) = map
    let ( <&> ) x f = map f x
    let ( <$ ) x tx = (map % const) x tx
    let ( $> ) tx x = x <$ tx
  end

  include Infix
end
