type 'a t =
  { decr : 'a -> 'a
  ; incr : 'a -> 'a
  ; mutable state : 'a
  ; start : 'a }

let make ~decr ~incr start = {decr; incr; state = start; start}
let current clock = clock.state

let next clock =
  let () = clock.state <- clock.incr clock.state in
  clock.state
;;

let previous clock =
  let () = clock.state <- clock.decr clock.state in
  clock.state
;;

let reset clock = clock.state <- clock.start
let int () = make ~decr:Pervasives.pred ~incr:Pervasives.succ 0
