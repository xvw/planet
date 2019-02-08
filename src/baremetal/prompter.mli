(** Read data from [stdin].
    
    Display stylizable prompter on stdout : 
    {[
      let open Baremetal in 
      let name = 
        Prompter.string ~f:String.capitalize_ascii "What's your name?"
      in print_endline ("Hello " ^  name)
    ]}
*)

(** {2 Aliases} *)

type question = string
type answer = string

(** {2 Prompters} *)

(** Display a generic prompter. *)
val generic :
     ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?question_style:Ansi.fragments
  -> (answer -> 'a)
  -> question
  -> 'a

(** Display a string prompter *)
val string :
     ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?question_style:Ansi.fragments
  -> ?f:(answer -> string)
  -> question
  -> string

(** Display a non-empty string prompter *)
val string_opt :
     ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?question_style:Ansi.fragments
  -> ?f:(answer option -> string option)
  -> question
  -> string option

(** Display an int prompter *)
val int :
     ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?question_style:Ansi.fragments
  -> ?f:(int -> int)
  -> ?default:int
  -> question
  -> int

(** Display an optional-int prompter *)
val int_opt :
     ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?question_style:Ansi.fragments
  -> ?f:(int option -> int option)
  -> question
  -> int option
