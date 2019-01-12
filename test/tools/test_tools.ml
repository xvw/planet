let test name callback = name, `Quick, callback
let round_trip f g x = x |> f |> g

let is_uniq_element elements base =
  elements
  |> List.fold_left (fun i elt -> if elt = base then succ i else i) 0
  |> fun x -> x = 1
;;

let all_are_uniq li =
  let rec aux = function
    | [] ->
      true
    | x :: xs ->
      if is_uniq_element li x then aux xs else false
  in
  aux li
;;
