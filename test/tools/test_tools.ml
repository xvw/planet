let test name callback = name, `Quick, callback
let round_trip f g x = x |> f |> g

let is_uniq_element elements base =
  elements
  |> List.fold_left (fun i elt -> if elt = base then succ i else i) 0
  |> fun x -> x = 1
;;

let all_are_uniq li =
  let rec aux = function
    | [] -> true
    | x :: xs -> if is_uniq_element li x then aux xs else false
  in
  aux li
;;

module Check = struct
  let color = Alcotest.testable Paperwork.Color.pp Paperwork.Color.eq
  let error = Alcotest.testable Bedrock.Error.pp Bedrock.Error.eq
  let validation left = Alcotest.(result left (list error))
end

module Testable = struct
  let check_result ty message a b =
    let r okt =
      match okt with
      | Ok x -> Ok x
      | Error y -> Error (Bedrock.Error.to_string y)
    in
    Alcotest.(check (result ty string) message (r a) (r b))
  ;;
end
