let id x = x
let flip f x y = f y x
let const x _ = x
let compose f g x = g (f x)
let ( $ ) f x = f x
let ( %> ) = compose
let ( <% ) f g x = f (g x)
let ( % ) = ( <% )
let md5 = Digest.(to_hex % string)
let bound value a b = min (max value a) b

let rec try_until predicate effect =
  let result = effect () in
  if predicate result then result else try_until predicate effect
;;
