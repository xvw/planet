(** Format for time serialization *)

open Bedrock

(** {2 Types} *)

type year
type month
type day
type hour
type min

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

(** Build a [year] *)
val year : int -> year Result.t

(** Build a [month] *)
val month : year -> Month.t -> month Result.t

(** Build a [day] *)
val day : month -> int -> day Result.t

(** Build a [day] all-in *)
val day_with : int -> Month.t -> int -> day Result.t
