open Bedrock
open Util

type question = string
type answer = string

let generic : type a.
       ?prefix:Ansi.fragments
    -> ?box_style:Ansi.fragments
    -> ?title_style:Ansi.fragments
    -> ?text_style:Ansi.fragments
    -> ?question_style:Ansi.fragments
    -> (answer -> a)
    -> question
    -> a =
 fun ?(prefix = Ansi.[!"│"])
     ?(box_style = Ansi.[fg cyan])
     ?(title_style = Ansi.[bold])
     ?(text_style = [])
     ?(question_style = [])
     callback
     question ->
  let () =
    Ansi.(
      text_box
        ~prefix
        ~box_style
        ~title_style
        ~text_style
        "prompter"
        question
      @ (reset :: box_style) @ [!"?"])
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
    ?(f = fun x -> x) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    f
;;

let string_opt
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(f = fun x -> x) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    (opt %> f)
;;

let int
    ?(prefix = Ansi.[!"│"])
    ?(box_style = Ansi.[fg cyan])
    ?(title_style = Ansi.[bold])
    ?(text_style = [])
    ?(question_style = [])
    ?(f = fun x -> x)
    ?(default = 0) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
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
    ?(f = fun x -> x) =
  generic
    ~prefix
    ~box_style
    ~title_style
    ~text_style
    ~question_style
    (int_of_string_opt %> f)
;;
