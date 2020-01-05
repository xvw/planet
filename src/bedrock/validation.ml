type 'a t = ('a, Error.t list) result
type 'a st = 'a t

let pop f = function
  | Ok x -> x
  | Error err -> f err
;;

let is_valid = function
  | Ok _ -> true
  | Error _ -> false
;;

let from_result = function
  | Ok x -> Ok x
  | Error x -> Error [ x ]
;;

let from_option error = function
  | Some x -> Ok x
  | None -> Error [ error ]
;;

module Functor = Functor.Make (struct
  type 'a t = 'a st

  let pure x = Ok x

  let map f = function
    | Error x -> Error x
    | Ok x -> Ok (f x)
  ;;
end)

module Monad = struct
  module M = Monad.Make_with_bind (struct
    type 'a t = 'a st

    let return x = Ok x

    let bind f = function
      | Error x -> Error x
      | Ok x -> f x
    ;;
  end)

  include M
  include (List.Monad.Traversable (M) : Sigs.TRAVERSABLE with type 'a t := 'a t)
end

module Applicative = struct
  module A = Applicative.Make (struct
    type 'a t = 'a st

    let pure x = Ok x

    let ap af ax =
      match af, ax with
      | Ok f, Ok x -> Ok (f x)
      | Error a, Error b -> Error (a @ b)
      | Error a, _ | _, Error a -> Error a
    ;;
  end)

  include A
  include (List.Applicative.Traversable (A) : Sigs.TRAVERSABLE with type 'a t := 'a t)
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

(** Instance inclusion *)

include Functor.Api
include Monad.Api
include Applicative.Api
include Infix
include Syntax
