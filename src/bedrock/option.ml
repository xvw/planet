type 'a t = 'a option

let eq f left right =
  match left, right with
  | None, None -> true
  | Some x, Some y -> f x y
  | _ -> false
;;

let pp pp' formater = function
  | None -> Format.fprintf formater "None"
  | Some x -> Format.fprintf formater "Some (%a)" pp' x
;;

let is_valid = function
  | Some _ -> true
  | None -> false
;;

let get_or f = function
  | None -> f ()
  | Some r -> r
;;

let to_list = function
  | None -> []
  | Some x -> [ x ]
;;

module Functor = Functor.Make (struct
  type 'a t = 'a option

  let pure x = Some x

  let map f = function
    | None -> None
    | Some x -> Some (f x)
  ;;
end)

module Monad = struct
  module M = Monad.Make_with_bind (struct
    type 'a t = 'a option

    let return x = Some x

    let bind f = function
      | None -> None
      | Some x -> f x
    ;;
  end)

  include M
  include (List.Monad.Traversable (M) : Sigs.TRAVERSABLE with type 'a t := 'a t)
end

module Applicative = struct
  module A = Applicative.Make_from_monad (Monad)
  include A

  include (
    List.Applicative.Traversable (A) : Sigs.TRAVERSABLE with type 'a t := 'a t)
end

module Infix = struct
  include Functor.Infix
  include Monad.Infix
  include Applicative.Infix
end

module Syntax = struct
  include Monad.Syntax
  include Applicative.Syntax
end

include Functor.Api
include Monad.Api
include Applicative.Api
include Infix
include Syntax
