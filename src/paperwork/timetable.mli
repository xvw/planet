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

module Year : sig
  (** Refences a year, between [2000] and [2999] and uses 
      3 digits : [000] for [2000], [999] for [2999]. For example, 
      [167] references [2167]. 
  *)
  type t

  (** Try to build a [Year.t]. *)
  val make : int -> t Result.t

  (** Check if a year is Leap or not *)
  val is_leap : t -> bool

  (** Serialize a [Year.t]. *)
  val to_string : t -> string

  (** Unserialize a [Year.t]. *)
  val from_string : string -> t Result.t

  (** Pretty printer *)
  val pp : Format.formatter -> t -> unit

  (** Equality *)
  val eq : t -> t -> bool
end

module Month : sig
  (** [month] : is the conjunction of a [year] and a [month], 
      referenced in [Month.t]. A month, in [string] is an 
      Upcase character from [A] to [L], [A] for {b January}, and 
      [L] for {b December}. For example, [019B] references 
      [February 2019]. 
  *)
  type t

  (** Month representation *)
  type month =
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

  (** Try to build a [Month.t]. *)
  val make : Year.t -> month -> t Result.t

  (** Get the number of days in a month. *)
  val days_in : t -> int

  (** Serialize a [Month.t]. *)
  val to_string : t -> string

  (** Unserialize a [Month.t]. *)
  val from_string : string -> t Result.t

  (** Get [Month.month] from int. *)
  val from_int : int -> month Result.t

  (** Pretty printer *)
  val pp : Format.formatter -> t -> unit

  (** Equality *)
  val eq : t -> t -> bool
end

module Day : sig
  (** Is the conjunction of a [month] and a [day], from [1]
      to [28], [29], [30] or [31] (depending on the [month] and the 
      [year]). For example : [019B22] references [2019 February 22th].
  *)
  type t

  (** Try to build a [Day.t]. *)
  val make : Month.t -> int -> t Result.t

  (** Try to build a [Day.t] with all values. *)
  val make_with : int -> Month.month -> int -> t Result.t

  (** Serialize a [Day.t]. *)
  val to_string : t -> string

  (** Unserialize a [Day.t]. *)
  val from_string : string -> t Result.t

  (** Pretty printer *)
  val pp : Format.formatter -> t -> unit

  (** Equality *)
  val eq : t -> t -> bool
end

module Hour : sig
  (** Is a tuple of the hour (from [0] to [23]) and the minuts
      (from [0] to [59]). The representation in string encodes the 
      Hour from [1] to [12] with a suffix : [AM] or [PM]. For example, 
      [11PM03] references [23:03], and [07AM12] references [7:12].
  *)
  type t

  (** Try to build an [Hour.t] *)
  val make : int -> int -> t Result.t

  (** Serialize an [Hour.t]. *)
  val to_string : t -> string

  (** Unserialize an [Hour.t]. *)
  val from_string : string -> t Result.t

  (** Pretty printer *)
  val pp : Format.formatter -> t -> unit

  (** Equality *)
  val eq : t -> t -> bool
end

module Moment : sig
  (** Is a tuple of a [day] and an [hour]. For example, 
    the [string] [019C07:06PM23] references the point : 
    [2019 February 07th, at 18:23].
  *)
  type t

  (** Build a [Moment.t]. *)
  val make : Day.t -> Hour.t -> t

  (** Build a [Moment.t] with all values. *)
  val make_with :
    int -> Month.month -> int -> int -> int -> t Result.t

  (** Serialize a [Moment.t]. *)
  val to_string : t -> string

  (** Unserialize a [Moment.t]. *)
  val from_string : string -> t Result.t

  (** Pretty printer *)
  val pp : Format.formatter -> t -> unit

  (** Equality *)
  val eq : t -> t -> bool

  (** Extract info *)
  val extract : t -> Year.t * Month.t * Day.t * Hour.t
end
