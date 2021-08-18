open Bedrock
open Alcotest
open Test_tools

module type DRIVER = sig
  module M : Sigs.Monad.API

  type h

  val name : string
  val sample_monad : h M.t list
  val sample_values : h list
  val check : string -> h M.t -> h M.t -> unit
  val f : h -> h M.t
  val g : h -> h M.t
end

module Driver (D : DRIVER) : sig
  val suite : (string * [> `Quick ] * (unit -> unit)) list
end = struct
  let str = D.name ^ " monad"
  let same = D.check ("same " ^ str)

  let law_1 elt =
    let left = D.M.(return elt >>= D.f) in
    let right = D.f elt in
    same left right
  ;;

  let law_2 right =
    let left = D.M.(right >>= return) in
    same left right
  ;;

  let law_3 elt =
    let left = D.M.(elt >>= D.f >>= D.g) in
    let right = D.M.(elt >>= fun x -> D.f x >>= D.g) in
    same left right
  ;;

  let sampling_values f () = List.iter f D.sample_values
  let sampling_monads f () = List.iter f D.sample_monad

  let suite =
    [ test
        ("[Law 1: " ^ str ^ "]: (return x >>= f) = f x")
        (sampling_values law_1)
    ; test ("[Law 2: " ^ str ^ "]: (m >>= return) = m") (sampling_monads law_2)
    ; test
        ("[Law 3: " ^ str ^ "]: ((m >= f) >= g) = (m >= (fun x → f x >= g))")
        (sampling_monads law_3)
    ]
  ;;
end

module IntList = Driver (struct
  module M = List.Monad

  type h = int

  let name = "int list"
  let check = Alcotest.check (list int)
  let f x = [ succ x ]
  let g x = [ x + 3456 ]

  let sample_values =
    [ 1
    ; 2
    ; 3
    ; 4
    ; 0
    ; -1
    ; 12
    ; 24
    ; 37890
    ; 123
    ; 79
    ; -456789
    ; 3456789
    ; 567
    ; 56789
    ; -45678
    ]
  ;;

  let sample_monad =
    [ [ 1; 2; 3; 4; 5 ]
    ; []
    ; [ 1 ]
    ; [ 567; 5678; 9876; 3456; 56; 5678 ]
    ; [ -1; -2; -3 ]
    ; [ -1; 0; 2; 56 ]
    ]
  ;;
end)

module IntArray = Driver (struct
  module M = Array.Monad

  type h = int

  let name = "int array"
  let check = Alcotest.check (array int)
  let f x = [| succ x |]
  let g x = [| x + 3456 |]

  let sample_values =
    [ 1
    ; 2
    ; 3
    ; 4
    ; 0
    ; -1
    ; 12
    ; 24
    ; 37890
    ; 123
    ; 79
    ; -456789
    ; 3456789
    ; 567
    ; 56789
    ; -45678
    ]
  ;;

  let sample_monad =
    [ [| 1; 2; 3; 4; 5 |]
    ; [||]
    ; [| 1 |]
    ; [| 567; 5678; 9876; 3456; 56; 5678 |]
    ; [| -1; -2; -3 |]
    ; [| -1; 0; 2; 56 |]
    ]
  ;;
end)

module StringList = Driver (struct
  module M = List.Monad

  type h = string

  let name = "string list"
  let check = Alcotest.check (list string)
  let f x = [ String.lowercase_ascii x ]
  let g x = [ String.uppercase_ascii x ]
  let sample_values = [ "foo"; ""; "bar"; "baz"; "FOO" ]
  let sample_monad = [ []; [ "foo"; "bar"; "baz" ]; [ ""; ""; "f"; "F" ] ]
end)

module StringArray = Driver (struct
  module M = Array.Monad

  type h = string

  let name = "string array"
  let check = Alcotest.check (array string)
  let f x = [| String.lowercase_ascii x |]
  let g x = [| String.uppercase_ascii x |]
  let sample_values = [ "foo"; ""; "bar"; "baz"; "FOO" ]
  let sample_monad = [ [||]; [| "foo"; "bar"; "baz" |]; [| ""; ""; "f"; "F" |] ]
end)

module IntOption = Driver (struct
  module M = Option.Monad

  type h = int

  let name = "int option"
  let check = Alcotest.check (option int)
  let f x = Some (x + 1)
  let g _ = None

  let sample_values =
    [ 1
    ; 2
    ; 3
    ; 4
    ; 0
    ; -1
    ; 12
    ; 24
    ; 37890
    ; 123
    ; 79
    ; -456789
    ; 3456789
    ; 567
    ; 56789
    ; -45678
    ]
  ;;

  let sample_monad = [ None; Some 10; Some (-23); Some 0; None; Some (-7890) ]
end)

module StringOption = Driver (struct
  module M = Option.Monad

  type h = string

  let name = "string option"
  let check = Alcotest.check (option string)
  let f x = Some (String.lowercase_ascii x)
  let g _ = None
  let sample_values = [ "foo"; ""; "bar"; "baz"; "FOO" ]
  let sample_monad = [ None; Some "foo"; Some "bar"; Some "baz"; Some "F" ]
end)

module IntResult = Driver (struct
  module M = Result.Monad

  type h = int

  let name = "int result"
  let check = Testable.check_result int
  let f x = Ok (x + 1)
  let g _ = Error (Error.Unknown "")

  let sample_values =
    [ 1
    ; 2
    ; 3
    ; 4
    ; 0
    ; -1
    ; 12
    ; 24
    ; 37890
    ; 123
    ; 79
    ; -456789
    ; 3456789
    ; 567
    ; 56789
    ; -45678
    ]
  ;;

  let sample_monad = [ g (); Ok 10; Ok (-23); Ok 0; g (); Ok (-7890) ]
end)

module StringResult = Driver (struct
  module M = Result.Monad

  type h = string

  let name = "string result"
  let check = Testable.check_result string
  let f x = Ok (String.lowercase_ascii x)
  let g _ = Ok "foo"
  let sample_values = [ "foo"; ""; "bar"; "baz"; "FOO" ]

  let sample_monad =
    [ Error (Error.Unknown ""); Ok "foo"; Ok "bar"; Ok "baz"; Ok "F" ]
  ;;
end)

let suite =
  IntList.suite
  @ IntArray.suite
  @ StringList.suite
  @ StringArray.suite
  @ IntOption.suite
  @ StringOption.suite
  @ IntResult.suite
  @ StringResult.suite
;;
