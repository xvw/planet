(** Generic Console Binding. *)

open Bedrock

(** {2 API} *)

(** {3 Log/Print} *)

val log : 'a -> unit
(** Log value on [console]. *)

val print : string -> unit
(** Print [string] on [console]. *)

val clear : unit -> unit
(** Clear [console]. *)

val info : 'a -> unit
(** Log info on [console]. *)

val error : 'a -> unit
(** Log error on [console]. *)

val warning : 'a -> unit
(** Log warning on [console]. *)

val dir : 'a -> unit
(** Display a JavaScript object whose properties should be output. *)

val trace : unit -> unit
(** Outputs a stack trace. *)

val table : ?columns:string list -> 'a -> unit
(** Display a table on the [console]. *)

(** {3 Counters} *)

val count : ?label:string -> unit -> unit
(** If supplied, [Console.count ~label ()] outputs the number of times it has
    been called with that label. If omitted, [count] behaves as though it was
    called with the ["default"] label. *)

val count_reset : ?label:string -> unit -> unit
(** If supplied, [count_reset] resets the count for that [label] to 0. If
    omitted, [count_reset] resets the ["default"] counter to 0. *)

(** {3 Timers} Timers are used to calculate the procedure execution time. - We
    instantiate a timer with: [Console.time name], where [name] is a unique
    timer ID; - [Console.timer_log name] displays the time elapsed since calling
    [Console.timer name]; - [Console.time_stop name] stops the timer, referenced
    by its name, in progress. *)

val time : string -> unit
val time_log : string -> 'a -> unit
val time_end : string -> unit

val timetrack : string -> (('a -> unit) -> unit) list -> unit
(** [Console.timetrack name actions] is a shortcut, for example: {[ let () =
    Console.timetrack "answer time" [ (fun logger -> logger (); Console.print
    "Hello") ; (fun logger -> logger (); Console.print "World") ] ;; ]} Where
    [logger] is a [Console.time_log]. This shortcut avoid the to instanciate and
    closed a timer. *)

(** {3 Groups} You can use nested groups to help organize your output by
    visually combining related material. To create a new nested block, call
    [Console.group ()].

    To exit the current group, simply call [Console.group_end ()]. *)

val group : ?label:'a -> unit -> unit
(** Creates a new inline group in the [console]. *)

val group_end : unit -> unit
(** Exits the current inline group in the [console]. *)

val render_error : Error.t list -> unit
(** render errors *)

val dump_errors : 'a -> Error.t list -> unit
(** Generic printer *)
