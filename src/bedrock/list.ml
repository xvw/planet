module L = Stdlib.List

let zip left right =
  try Some (L.map2 (fun x y -> x, y) left right) with _ -> None
;;

let eq f left right =
  match zip left right with
  | None ->
    false
  | Some l ->
    L.for_all (fun (x, y) -> f x y) l
;;

let ( @? ) left = function None -> left | Some right -> left @ right

let hds index list =
  let rec aux acc i = function
    | [] ->
      Stdlib.List.rev acc
    | x :: xs ->
      if i <= 0 then Stdlib.List.rev (x :: acc) else aux (x :: acc) (pred i) xs
  in
  aux [] (pred index) list
;;

module Functor = Functor.Make (struct
  type 'a t = 'a list

  let pure x = [ x ]
  let map f x = Stdlib.List.map f x
end)

module Monad = struct
  include Monad.Make_with_join (struct
    type 'a t = 'a list

    let return x = [ x ]
    let map = Stdlib.List.map
    let join = Stdlib.List.concat
  end)

  module Traversable (M : Sigs.Monad.API) = struct
    type 'a t = 'a M.t

    let traverse =
      let open M.Infix in
      let rec aux f = function
        | [] ->
          M.return []
        | x :: xs ->
          f x >>= fun h -> aux f xs >>= fun t -> M.return (Stdlib.List.cons h t)
      in
      aux
    ;;

    let sequence x = traverse Util.id x
  end
end

module Applicative = struct
  include Applicative.Make_from_monad (Monad)

  module Traversable (A : Sigs.Applicative.API) = struct
    type 'a t = 'a A.t

    let traverse =
      let open A.Infix in
      let rec aux f = function
        | [] ->
          A.pure []
        | x :: xs ->
          Stdlib.List.cons <$> f x <*> aux f xs
      in
      aux
    ;;

    let sequence x = traverse Util.id x
  end
end

include L

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
