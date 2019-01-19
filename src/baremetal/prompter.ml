open Bedrock
open Util

type question = string
type answer = string

let generic : type a.
       ?question_style:Ansi.fragments
    -> ?answer_style:Ansi.fragments
    -> (answer -> a)
    -> question
    -> a =
 fun ?(question_style = Ansi.[bold])
     ?(answer_style = [])
     callback
     question ->
  let qs = Ansi.only_style question_style in
  let an = Ansi.reset :: Ansi.only_style answer_style in
  let () = Format.printf "%a%s" Ansi.pp qs question in
  let () = Format.printf "%a@." Ansi.pp an in
  let result = Stdlib.read_line () in
  let () = Format.printf "%a" Ansi.pp [Ansi.reset] in
  callback result
;;

let opt = function "" -> None | x -> Some x

let string
    ?(question_style = Ansi.[bold])
    ?(answer_style = [])
    ?(f = fun x -> x) =
  generic ~question_style ~answer_style f
;;

let string_opt
    ?(question_style = Ansi.[bold])
    ?(answer_style = [])
    ?(f = fun x -> x) =
  generic ~question_style ~answer_style (opt %> f)
;;

let int
    ?(question_style = Ansi.[bold])
    ?(answer_style = [])
    ?(f = fun x -> x)
    ?(default = 0) =
  generic ~question_style ~answer_style (fun x ->
      match int_of_string_opt x with
      | None ->
        f default
      | Some x ->
        f x )
;;

let int_opt
    ?(question_style = Ansi.[bold])
    ?(answer_style = [])
    ?(f = fun x -> x) =
  generic ~question_style ~answer_style (int_of_string_opt %> f)
;;
