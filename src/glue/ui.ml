open Bedrock
open Util
open Baremetal

let prompt_errors ?(intro = true) errors =
  let () =
    if intro
    then
      Ansi.[foreground red; bold; text "Errors are occured:"]
      |> Ansi.to_string ~scoped:true
      |> print_endline
  in
  let () =
    List.iter
      (fun error -> Format.printf "  - %s@." (Error.to_string error))
      errors
  in
  ()
;;

let prompt_error ?(intro = true) error = prompt_errors ~intro [error]

let link_box
    ?(prefix = Ansi.[!"│+ "])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(f =
      fun (name, url) ->
        Ansi.
          [ fg green
          ; !name
          ; reset
          ; !"  <"
          ; fg yellow
          ; underline
          ; !url
          ; reset
          ; !">" ])
    title
    list =
  let maxlen =
    List.fold_left
      (fun acc (x, _) -> max acc $ String.length x)
      0
      list
  in
  let nl =
    List.map
      (fun (a, b) ->
        a ^ (String.make $ maxlen - String.length a $ ' '), b )
      list
  in
  Ansi.(generic_box ~prefix ~box_style ~title_style) f title nl
;;

let dated_link_box
    ?(prefix = Ansi.[!"│+ "])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(f =
      fun (name, date, url) ->
        Ansi.
          [ fg green
          ; !name
          ; reset
          ; !"   "
          ; fg black
          ; bg cyan
          ; !" "
          ; !(Paperwork.Timetable.Day.to_string date)
          ; !" "
          ; reset
          ; !"  <"
          ; fg yellow
          ; underline
          ; !url
          ; reset
          ; !">" ])
    title
    list =
  let maxlen =
    List.fold_left
      (fun acc (x, _, _) -> max acc $ String.length x)
      0
      list
  in
  let nl =
    List.map
      (fun (a, b, c) ->
        a ^ (String.make $ maxlen - String.length a $ ' '), b, c )
      list
  in
  Ansi.(generic_box ~prefix ~box_style ~title_style) f title nl
;;
