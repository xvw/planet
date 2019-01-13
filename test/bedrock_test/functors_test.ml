open Bedrock
open Alcotest
open Test_tools
open Util

module type DRIVER = sig
  module F : Functor.API

  type h

  val name : string
  val sample : h F.t list
  val test_driver : h F.t testable
  val f : h -> h
  val g : h -> h
end

module Driver (D : DRIVER) : sig
  val suite : (string * [> `Quick] * (unit -> unit)) list
end = struct
  let str = D.name ^ " functor"
  let same = check D.test_driver ("same " ^ str)

  let law_1 left =
    let right = D.F.map id left in
    same left right
  ;;

  let law_2 elt =
    let left = D.F.map (D.f % D.g) elt in
    let right = (D.F.map D.f % D.F.map D.g) elt in
    same left right
  ;;

  let sampling f () = List.iter f D.sample

  let suite =
    [ test ("[Law 1: " ^ str ^ "]: (map id x) = x") (sampling law_1)
    ; test
        ("[Law 2: " ^ str ^ "]: (map (f . g)) =  ((map f) . (map g))")
        (sampling law_2) ]
  ;;
end

module IntList = Driver (struct
  module F = Functors.List

  type h = int

  let name = "int list"
  let test_driver = list int
  let f x = x + 2
  let g x = x - 12

  let sample =
    [ [1; 2; 3; 4; 5]
    ; []
    ; [1]
    ; [567; 5678; 9876; 3456; 56; 5678]
    ; [-1; -2; -3]
    ; [-1; 0; 2; 56] ]
  ;;
end)

module StringList = Driver (struct
  module F = Functors.List

  type h = string

  let name = "string list"
  let test_driver = list string
  let f = String.uppercase_ascii
  let g = String.lowercase_ascii

  let sample =
    [ ["Foo"; "Bar"; "Baz"]
    ; []
    ; ["1"]
    ; ["aaaa"; "bbb"]
    ; ["A"; "B"; "C"; "D"; "EEEE"; "FFF"] ]
  ;;
end)

module IntArray = Driver (struct
  module F = Functors.Array

  type h = int

  let name = "int array"
  let test_driver = array int
  let f x = x + 2
  let g x = x - 12

  let sample =
    [ [|1; 2; 3; 4; 5|]
    ; [||]
    ; [|1|]
    ; [|567; 5678; 9876; 3456; 56; 5678|]
    ; [|-1; -2; -3|]
    ; [|-1; 0; 2; 56|] ]
  ;;
end)

module StringArray = Driver (struct
  module F = Functors.Array

  type h = string

  let name = "string array"
  let test_driver = array string
  let f = String.uppercase_ascii
  let g = String.lowercase_ascii

  let sample =
    [ [|"Foo"; "Bar"; "Baz"|]
    ; [||]
    ; [|"1"|]
    ; [|"aaaa"; "bbb"|]
    ; [|"A"; "B"; "C"; "D"; "EEEE"; "FFF"|] ]
  ;;
end)

module StringOption = Driver (struct
  module F = Functors.Option

  type h = string

  let name = "string option"
  let test_driver = option string
  let f = String.uppercase_ascii
  let g = String.lowercase_ascii
  let sample = [Some "foo"; None; Some "Bar"; Some ""]
end)

module IntOption = Driver (struct
  module F = Functors.Option

  type h = int

  let name = "int option"
  let test_driver = option int
  let f = pred
  let g = succ
  let sample = [Some 1; None; Some 1200; Some 0]
end)

let suite =
  IntList.suite
  @ StringList.suite
  @ StringOption.suite
  @ IntOption.suite
  @ IntArray.suite
  @ StringArray.suite
;;
