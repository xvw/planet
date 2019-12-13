open Bedrock
open Alcotest
open Test_tools

let bound_1 () = check int "same ints" 10 (Util.bound 10 0 15)
let bound_2 () = check int "same ints" 0 (Util.bound (-28) 0 15)
let bound_3 () = check int "same ints" 15 (Util.bound 28 0 15)

let md5_1 () =
  check string "same strings" "acbd18db4cc2f85cedef654fccc4a4d8"
    (Util.md5 "foo")
;;

let md5_2 () =
  check string "same strings" "cfc0d4d9009e00129baae0678fe6f52c"
    (Util.md5 "Foo/Bar")
;;

let hds_1 () =
  check (list int) "same lists" [ 1; 2; 3 ] (List.hds 3 [ 1; 2; 3; 4; 5 ])
;;

let hds_2 () = check (list int) "same lists" [ 1 ] (List.hds 3 [ 1 ])
let hds_3 () = check (list int) "same lists" [] (List.hds 3 [])

let suite =
  [ test "[bound] in simple case" bound_1
  ; test "[bound] in lower case" bound_2
  ; test "[bound] in upper case" bound_3
  ; test "[md5] case 1" md5_1
  ; test "[md5] case 2" md5_2
  ; test "[List.hds] case 1" hds_1
  ; test "[List.hds] case 2" hds_2
  ; test "[List.hds] case 3" hds_3
  ]
;;
