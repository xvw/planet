(** Formatting and printing for ANSI term. *)

(** {2 Types} *)

(** Describe a color *)
type color

(** Describe an ANSI style *)
type fragment

(** {2 Colors} 
    [Color] is defined to be used with fragment.
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

val reset : fragment
val bold : fragment
val underline : fragment
val blink : fragment
val inverse : fragment
val hidden : fragment

(** Convert a [string] to a [fragment]. *)
val text : string -> fragment

(** Same of [text]. *)
val ( ! ) : string -> fragment

(** Convert a [color] into a [foreground fragment].*)
val foreground : color -> fragment

(** Same of [foreground]. *)
val fg : color -> fragment

(** Convert a [color] into a [background fragment].*)
val background : color -> fragment

(** Same of [background]. *)
val bg : color -> fragment

(** Fragment composition. *)
val merge : fragment -> fragment -> fragment

(** Same of [merge]. *)
val ( & ) : fragment -> fragment -> fragment

(** {2 String generation and Printing} *)

(** Convert [fragment] to [string]. *)
val to_string : ?scoped:bool -> fragment -> string

(** [Pretty printer] to deal with [Fromat] module. *)
val pp : Format.formatter -> fragment -> unit

(** [Pretty printer] to deal with [Fromat] module with scoped fragment. *)
val pps : Format.formatter -> fragment -> unit
