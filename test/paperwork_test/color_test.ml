open Bedrock
open Paperwork
open Test_tools

let parse_some_colors () =
  let subjects =
    [ Color.create 255 255 255, "rgb(255, 255, 255)", "#ffffff"
    ; Color.create 0 0 0, "rgb(0, 0, 0)", "#000000"
    ; Color.create 13 12 300, "rgb(13, 12, 255)", "#0d0cff"
    ; Color.create (-1000) 12 300, "rgb(0, 12, 255)", "#000cff"
    ; ( Color.create ~alpha:1.0 (-1000) 12 300
      , "rgb(0, 12, 255)"
      , "#000cff" )
    ; ( Color.create ~alpha:0.6 (-1000) 12 300
      , "rgba(0, 12, 255, 0.6)"
      , "#000cff" ) ]
  in
  List.iter
    (fun (color, rgb, hex) ->
      let a = Color.to_rgb color
      and b = Color.to_hex color in
      let open Alcotest in
      let () = check string "same strings (rgb)" a rgb in
      check string "same strings (hex)" b hex )
    subjects
;;

let suite =
  [ test
      "[color creation and serialization] some random cases"
      parse_some_colors ]
;;
