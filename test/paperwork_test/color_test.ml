open Bedrock
open Error
open Paperwork
open Test_tools

let produce_some_colors () =
  let subjects =
    [ (Color.create 255 255 255, "rgb(255, 255, 255)", "#ffffff")
    ; (Color.create 0 0 0, "rgb(0, 0, 0)", "#000000")
    ; (Color.create 13 12 300, "rgb(13, 12, 255)", "#0d0cff")
    ; (Color.create (-1000) 12 300, "rgb(0, 12, 255)", "#000cff")
    ; (Color.create ~alpha:1.0 (-1000) 12 300, "rgb(0, 12, 255)", "#000cff")
    ; ( Color.create ~alpha:0.6 (-1000) 12 300
      , "rgba(0, 12, 255, 0.6)"
      , "#000cff" )
    ] in
  List.iter
    (fun (color, rgb, hex) ->
      let a = Color.to_rgb color
      and b = Color.to_hex color in
      let open Alcotest in
      let () = check string "same strings (rgb)" a rgb in
      check string "same strings (hex)" b hex)
    subjects
;;

let parse_some_colors () =
  let subject =
    [ ("#FFFFFF", Ok (Color.create 255 255 255))
    ; ("#000000", Ok (Color.create 0 0 0))
    ; ("rgb(3, 4, 5)", Ok (Color.create 3 4 5))
    ; ("rgba(3, 4, 5, 0.7)", Ok (Color.create ~alpha:0.7 3 4 5))
    ; ("ffffffffff", Error (Unparsable_color "ffffffffff"))
    ; ("rgb(3, 5, 7", Error (Unparsable_color "rgb(3, 5, 7"))
    ] in
  List.iter
    (fun (base, expected) ->
      let parsed = Color.from_string base in
      let open Alcotest in
      check (result Check.color Check.error) "same color" parsed expected)
    subject
;;

let suite =
  [ test "[color creation and serialization] some random cases"
      produce_some_colors
  ; test "[color parsing] some random cases" parse_some_colors
  ]
;;
