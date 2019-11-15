(** Read data from [stdin].
    
    Display stylizable prompter on stdout : 
    {[
      let open Baremetal in 
      let name = 
        Prompter.string ~f:String.capitalize_ascii "What's your name?"
      in print_endline ("Hello " ^  name)
    ]}
*)

open Bedrock

(** {2 Aliases} *)

type question = string
type answer = string

(** {2 Prompt errors} *)

val prompt_errors : ?intro:bool -> Error.t list -> unit
val prompt_error : ?intro:bool -> Error.t -> unit

(** {2 Prompters} *)

val flush : unit -> unit

(** Display a generic prompter. *)
val generic
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> (answer -> 'a)
  -> question
  -> 'a

(** Display a string prompter *)
val string
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> ?f:(answer -> string)
  -> question
  -> string

(** Display a non-empty string prompter *)
val string_opt
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> ?f:(answer option -> string option)
  -> question
  -> string option

(** Display an int prompter *)
val int
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> ?f:(int -> int)
  -> ?default:int
  -> question
  -> int

(** Display an optional-int prompter *)
val int_opt
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> ?f:(int option -> int option)
  -> question
  -> int option

(** Display an yes-no prompter *)
val yes_no
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> ?f:(answer -> bool)
  -> question
  -> bool

(** Display a prompter which could fail *)
val resultable
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> (string -> 'a Result.t)
  -> question
  -> 'a Result.t

(** Display a prompter which could fail with a validation *)
val validable
  :  ?prefix:Ansi.fragments
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:string
  -> ?bottom:Ansi.fragments
  -> (string -> 'a Validation.t)
  -> question
  -> 'a Validation.t

(** Perform a choice in an array *)
val choose
  :  ?prefix:Ansi.fragments
  -> ?choice_prefix:Ansi.fragment list
  -> ?choice_suffix:Ansi.fragment list
  -> ?choice_style:Ansi.fragment list
  -> ?box_style:Ansi.fragments
  -> ?title_style:Ansi.fragments
  -> ?text_style:Ansi.fragments
  -> ?answer_style:Ansi.fragments
  -> ?title:answer
  -> ?bottom:Ansi.fragment list
  -> ('a -> 'b)
  -> ('a -> answer)
  -> 'a array
  -> answer
  -> ('b, Error.t) result

val repeat_result : 'a Result.t -> bool
val repeat_validation : 'a Validation.t -> bool
val repeat_option : 'a Option.t -> bool
