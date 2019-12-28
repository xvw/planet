open Bedrock
open Util

type question = string
type answer = string

let prompt_errors ?(intro = true) errors =
  let () =
    if intro then
      Ansi.[ foreground red; bold; text "Errors are occured:" ]
      |> Ansi.to_string ~scoped:true
      |> print_endline in
  let () =
    List.iter
      (fun error -> Format.printf "  - %s@." (Error.to_string error))
      errors in
  ()
;;

let flush () =
  let open Ansi in
  let r = Format.asprintf "%a" Ansi.pp [ reset ] in
  print_string r
;;

let prompt_error ?(intro = true) error = prompt_errors ~intro [ error ]

let generic :
    type a.
       ?prefix:Ansi.fragments
    -> ?box_style:Ansi.fragments
    -> ?title_style:Ansi.fragments
    -> ?text_style:Ansi.fragments
    -> ?answer_style:Ansi.fragments
    -> ?title:string
    -> ?bottom:Ansi.fragments
    -> (answer -> a)
    -> question
    -> a =
 fun ?(prefix = Ansi.[ !"│" ]) ?(box_style = Ansi.[ fg cyan ])
     ?(title_style = Ansi.[ bold ]) ?(text_style = []) ?(answer_style = [])
     ?(title = "prompter") ?(bottom = Ansi.[ !"?" ]) callback question ->
   let () =
     Ansi.(
       text_box ~prefix ~box_style ~title_style ~text_style title question
       @ (reset :: box_style)
       @ bottom)
     |> Ansi.to_string
     |> print_string in
   let () = Format.printf "%a@." Ansi.pp answer_style in
   let result = Stdlib.read_line () in
   let () = flush () in
   callback result
;;

let opt = function "" -> None | x -> Some x

let string
    ?(prefix = Ansi.[ !"│" ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ !"?" ])
    ?(f = (fun x -> x)) =
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom f
;;

let string_opt
    ?(prefix = Ansi.[ !"│" ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ !"?" ])
    ?(f = (fun x -> x)) =
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom (opt %> f)
;;

let int
    ?(prefix = Ansi.[ !"│" ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ !"?" ])
    ?(f = (fun x -> x))
    ?(default = 0) =
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom (fun x ->
      (match int_of_string_opt x with None -> f default | Some x -> f x))
;;

let int_opt
    ?(prefix = Ansi.[ !"│" ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ !"?" ])
    ?(f = (fun x -> x)) =
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom (int_of_string_opt %> f)
;;

let yes_no
    ?(prefix = Ansi.[ !"│" ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ !"─[1.yes]─[2.no] " ])
    ?(f =
      fun x ->
        let res = String.trim (String.lowercase_ascii x) in
        res = "y" || res = "yes" || res = "1") =
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom f
;;

let resultable
    ?(prefix = Ansi.[ !"│" ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ fg red; !"?" ])
    f =
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom f
;;

let validable
    ?(prefix = Ansi.[ !"│" ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ fg red; !"?" ])
    f =
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom f
;;

let choose
    ?(prefix = Ansi.[ !"│" ])
    ?(choice_prefix = Ansi.[ fg cyan; !"└─" ])
    ?(choice_suffix = Ansi.[ reset; !"\n" ])
    ?(choice_style = Ansi.[ fg cyan ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ !"?" ])
    f
    g
    choices =
  let len = Array.length choices in
  let st_choices =
    (List.init len (fun i ->
         let p = if i = 0 then [] else choice_prefix in
         let s =
           if i = pred len then
             Ansi.(choice_suffix @ choice_prefix @ [ reset ])
           else
             choice_suffix in
         choice_style
         @ p
         @ Ansi.[ !(Format.sprintf "%d.%s" $ i $ g choices.(i)) ]
         @ s)
    |> List.flatten
    )
    @ bottom in
  let real_f answer =
    match int_of_string_opt answer with
    | None ->
      Error (Error.Unknown ("invalid int: " ^ answer))
    | Some x -> (
      try Ok (choices.(x) |> f) with
      | _ ->
        Error (Error.Unknown "Invalid index")
    ) in
  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom:st_choices real_f
;;

let choose_multiple
    ?(prefix = Ansi.[ !"│" ])
    ?(choice_prefix = Ansi.[ fg cyan; !"└─" ])
    ?(choice_suffix = Ansi.[ reset; !"\n" ])
    ?(choice_style = Ansi.[ fg cyan ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(text_style = [])
    ?(answer_style = [])
    ?(title = "prompter")
    ?(bottom = Ansi.[ !"?" ])
    f
    g
    choices =
  let len = Array.length choices in
  let st_choices =
    (List.init len (fun i ->
         let p = if i = 0 then [] else choice_prefix in
         let s =
           if i = pred len then
             Ansi.(choice_suffix @ choice_prefix @ [ reset ])
           else
             choice_suffix in
         choice_style
         @ p
         @ Ansi.[ !(Format.sprintf "%d.%s" $ i $ g choices.(i)) ]
         @ s)
    |> List.flatten
    )
    @ bottom in
  let real_f answer =
    let l =
      String.split_on_char ',' answer
      |> List.map String.trim
      |> List.sort_uniq String.compare
      |> List.map int_of_string_opt
      |> Option.Applicative.sequence in
    match l with
    | None ->
      Error (Error.Unknown ("invalid int sequence: " ^ answer))
    | Some list ->
      let final_list =
        List.map
          (fun x ->
            try Ok (choices.(x) |> f) with
            | _ ->
              Error (Error.Of (Format.asprintf "Unknown index %d" x)))
          list
        |> Result.Applicative.sequence in
      final_list in

  generic ~prefix ~box_style ~title_style ~text_style ~answer_style ~title
    ~bottom:st_choices real_f
;;

let repeat_result = function
  | Ok _ ->
    true
  | Error e ->
    prompt_error e;
    false
;;

let repeat_validation = function
  | Ok _ ->
    true
  | Error e ->
    prompt_errors e;
    false
;;

let repeat_option = function
  | None ->
    prompt_error Error.(Invalid_field "input");
    false
  | Some _ ->
    true
;;
