(** Formatting and printing for ANSI term.

    This module expose too write "formatted text" in a terminal, using ANSI and
    ASCII. For example :

    {[ let open Baremetal.Ansi in let fragment = [ background red ; foreground
    black ; text "Hello" ; reset ; text ", " ; background black ; foreground red
    ; text "World" ] in fragment |> to_string |> print_endline ]} *)

(** {2 Types} *)

type color
(** Describe a color *)

type fragment
(** Describe an ANSI style *)

type fragments = fragment list
(** Describe a Sequence of [fragment] *)

(** {2 Colors} [Color] is defined to be used with fragments. *)

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

(** {2 Fragments} [Fragments] are "piece of formatted" text. They can be
    combined. *)

val reset : fragment
val bold : fragment
val underline : fragment
val blink : fragment
val inverse : fragment
val hidden : fragment
val erase_line : fragment
val erase_above : fragment
val erase_below : fragment
val erase_screen : fragment

val text : string -> fragment
(** Convert a [string] to a [fragments]. *)

val ( ! ) : string -> fragment
(** Same of [text]. *)

val foreground : color -> fragment
(** Convert a [color] into a [foreground fragments].*)

val fg : color -> fragment
(** Same of [foreground]. *)

val background : color -> fragment
(** Convert a [color] into a [background fragments].*)

val bg : color -> fragment
(** Same of [background]. *)

(** {2 String generation and Printing} *)

val to_string : ?scoped:bool -> fragments -> string
(** Convert [fragments] to [string]. *)

(** [Formatter] allow you to use "ANSI formatting" with [Format/Printf] module.
    For example :

    {[ let open Baremetal.Ansi in Format.printf "Formatted text [%a]" pps [fg
    magenta; text "Hello world"] ]} *)

val pp : Format.formatter -> fragments -> unit
(** [Pretty printer] to deal with [Fromat] module. *)

val pps : Format.formatter -> fragments -> unit
(** [Pretty printer] to deal with [Fromat] module with scoped fragments. *)

(** {2 Manage fragments} *)

val only_style : fragments -> fragments
(** Keep only style into a fragment *)

val box :
     ?prefix:fragments
  -> ?box_style:fragments
  -> ?title_style:fragments
  -> string
  -> fragments list
  -> fragments
(** Boxed fragment *)

val generic_box :
     ?prefix:fragments
  -> ?box_style:fragments
  -> ?title_style:fragments
  -> ('a -> fragments)
  -> string
  -> 'a list
  -> fragments
(** generic box fragment *)

val text_box :
     ?prefix:fragments
  -> ?box_style:fragments
  -> ?title_style:fragments
  -> ?text_style:fragments
  -> string
  -> string
  -> fragments
