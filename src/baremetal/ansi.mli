(** Formatting and printing for ANSI term. 

    This module expose too write "formatted text" in a terminal, using 
    ANSI and ASCII. For example : 

    {[
      let open Baremetal.Ansi in 
      let fragment = 
        background red 
        & foreground black 
        & text "Hello" 
        & reset 
        & text ", "
        & background black 
        & foreground red 
        & text "World"
      in
      fragment 
      |> to_string 
      |> print_endline
    ]}
*)

(** {2 Types} *)

(** Describe a color *)
type color

(** Describe an ANSI style *)
type fragment

(** Describe a Sequence of [fragment] *)
type fragments = fragment list

(** {2 Colors} 
    [Color] is defined to be used with fragments.
*)

val default : color
val black : color
val red : color
val green : color
val yellow : color
val blue : color
val magenta : color
val cyan : color
val white : color
val bright_black : color
val bright_red : color
val bright_green : color
val bright_yellow : color
val bright_blue : color
val bright_magenta : color
val bright_cyan : color
val bright_white : color

(** {2 Fragments} 
    [Fragments] are "piece of formatted" text. They can be 
    combined.
*)

val reset : fragments
val bold : fragments
val underline : fragments
val blink : fragments
val inverse : fragments
val hidden : fragments

(** Convert a [string] to a [fragments]. *)
val text : string -> fragments

(** Same of [text]. *)
val ( ! ) : string -> fragments

(** Convert a [color] into a [foreground fragments].*)
val foreground : color -> fragments

(** Same of [foreground]. *)
val fg : color -> fragments

(** Convert a [color] into a [background fragments].*)
val background : color -> fragments

(** Same of [background]. *)
val bg : color -> fragments

(** Fragments composition. *)
val merge : fragments -> fragments -> fragments

(** Same of [merge]. *)
val ( & ) : fragments -> fragments -> fragments

(** {2 String generation and Printing} *)

(** Convert [fragments] to [string]. *)
val to_string : ?scoped:bool -> fragments -> string

(** [Pretty printer] to deal with [Fromat] module. *)
val pp : Format.formatter -> fragments -> unit

(** [Pretty printer] to deal with [Fromat] module with scoped fragments. *)
val pps : Format.formatter -> fragments -> unit
