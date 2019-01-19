(** Format for time serialization.
    
    {2 Concept}

    The goal of this module is to provides a easy-way to 
    serialize [timepoints].

    [Timetable] provides 5 kind of timepoints, as abstract types. 
    (Essentially for checking the validity of a timepoint):

    - [year] : refences a year, between [2000] and [2999] and uses 
      3 digits : [000] for [2000], [999] for [2999]. For example, 
      [167] references [2167].

    - [month] : is the conjunction of a [year] and a [month], 
      referenced in [Month.t]. A month, in [string] is an 
      Upcase character from [A] to [L], [A] for {b January}, and 
      [L] for {b December}. For example, [019B] references 
      [February 2019].

    - [day] : is the conjunction of a [month] and a [day], from [1]
      to [28], [29], [30] or [31] (depending on the [month] and the 
      [year]). For example : [019B22] references [2019 February 22th].

    - [hour] : is a tuple of the hour (from [0] to [23]) and the minuts
      (from [0] to [59]). The representation in string encodes the 
      Hour from [1] to [12] with a suffix : [AM] or [PM]. For example, 
      [11PM03] references [23:03], and [07AM12] references [7:12].

    - [moment] : is a tuple of a [day] and an [hour]. For example, 
      the [string] [019C07:06PM23] references the point : 
      [2019 February 07th, at 18:23].

    The format does not handle the seconds (because ... it does not 
    need for my goals).
 *)

open Bedrock

(** {2 Types} *)

(** Refences a year, between [2000] and [2999] and uses 
    3 digits : [000] for [2000], [999] for [2999]. For example, 
    [167] references [2167]. 
*)
type year

(** Is the conjunction of a [year] and a [month], 
    referenced in [Month.t]. A month, in [string] is an 
    Upcase character from [A] to [L], [A] for {b January}, and 
    [L] for {b December}. For example, [019B] references 
    [February 2019]. 
*)
type month

(** Is the conjunction of a [month] and a [day], from [1]
    to [28], [29], [30] or [31] (depending on the [month] and the 
    [year]). For example : [019B22] references [2019 February 22th].
*)
type day

(** Is a tuple of the hour (from [0] to [23]) and the minuts
    (from [0] to [59]). The representation in string encodes the 
    Hour from [1] to [12] with a suffix : [AM] or [PM]. For example, 
    [11PM03] references [23:03], and [07AM12] references [7:12].
*)
type hour

(** Is a tuple of a [day] and an [hour]. For example, 
    the [string] [019C07:06PM23] references the point : 
    [2019 February 07th, at 18:23].
*)
type moment

(** {2 Month} 
    Enumerate months
*)

module Month : sig
  type t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

  val to_string : t -> string
  val from_int : int -> t Result.t
  val to_char : t -> char
  val from_char : char -> t Result.t
end

(** {2 Constructors} *)

(** Build a [year]. *)
val year : int -> year Result.t

(** Build a [month]. *)
val month : year -> Month.t -> month Result.t

(** Build a [day]. *)
val day : month -> int -> day Result.t

(** Build a [day] all-in. *)
val day_with : int -> Month.t -> int -> day Result.t

(** Build an [hour]. *)
val hour : int -> int -> hour Result.t

(** Build a [moment] with [day] and [hour]. *)
val moment : day -> hour -> moment

(** [moment_with year month day hour min] *)
val moment_with :
  int -> Month.t -> int -> int -> int -> moment Result.t

(** {2 Helpers} *)

(** Returns the numer of days in a month.*)
val days_in : month -> int

(** Returns [true] if a year is leap; [false] otherwise. *)
val is_leap : year -> bool

(** {2 Serialization} *)

val year_to_string : year -> string
val month_to_string : month -> string
val day_to_string : day -> string
val hour_to_string : hour -> string
val moment_to_string : moment -> string

(** {2 Serialization} *)

val year_from_string : string -> year Result.t
val month_from_string : string -> month Result.t
val day_from_string : string -> day Result.t
val hour_from_string : string -> hour Result.t
val moment_from_string : string -> moment Result.t
