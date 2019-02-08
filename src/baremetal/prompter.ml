open Bedrock
open Util

type question = string
type answer = string

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

let generic : type a.
       ?prefix:Ansi.fragments
    -> ?box_style:Ansi.fragments
    -> ?title_style:Ansi.fragments
    -> ?text_style:Ansi.fragments
    -> ?question_style:Ansi.fragments
    -> ?title:string
    -> ?bottom:Ansi.fragments
    -> (answer -> a)
    -> question
    -> a =
 fun ?(prefix = Ansi.[!"│"])
     ?(box_style = Ansi.[fg cyan])
     ?(title_style = Ansi.[bold])
     ?(text_style = [])
     ?(question_style = [])
     ?(title = "prompter")
     ?(bottom = Ansi.[!"?"])
     callback
     question ->
  let () =
    Ansi.(
      text_box
        ~prefix
        ~box_style
        ~title_style
        ~text_style
        title
        question
      @ (reset :: box_style) @ bottom)
    |> Ansi.to_string |> print_string
  in
  let () = Format.printf "%a@." Ansi.pp question_style in
  let result = Stdlib.read_line () in
  let () = Format.printf "%a" Ansi.pp [Ansi.reset] in
  callback result
;;

let opt = function "" -> None | x -> Some x

let string
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[!"?"])
    ?(f = fun x -> x) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    ~title
    ~bottom
    f
;;

let string_opt
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[!"?"])
    ?(f = fun x -> x) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    ~title
    ~bottom
    (opt %> f)
;;

let int
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[!"?"])
    ?(f = fun x -> x)
    ?(default = 0) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    ~title
    ~bottom
    (fun x ->
      match int_of_string_opt x with
      | None ->
        f default
      | Some x ->
        f x )
;;

let int_opt
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[!"?"])
    ?(f = fun x -> x) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    ~title
    ~bottom
    (int_of_string_opt %> f)
;;

let yes_no
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[!"─[1.yes]─[2.no] "])
    ?(f =
      fun x ->
        let res = String.trim (String.lowercase_ascii x) in
        res = "y" || res = "yes" || res = "1") =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    ~title
    ~bottom
    f
;;

let resultable
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[fg red; !"?"])
    f =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    ~title
    ~bottom
    f
;;

let validable
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[fg red; !"?"])
    f =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    ~title
    ~bottom
    f
;;
