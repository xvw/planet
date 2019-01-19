open Bedrock
open Error

type year = Year of int
type month = Month of (year * int)
type day = Day of (month * int)
type hour = Hour of int
type min = Min of int

module Month = struct
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

  let to_string = function
    | Jan ->
      "jan"
    | Feb ->
      "feb"
    | Mar ->
      "mar"
    | Apr ->
      "apr"
    | May ->
      "may"
    | Jun ->
      "jun"
    | Jul ->
      "jul"
    | Aug ->
      "aug"
    | Sep ->
      "sep"
    | Oct ->
      "oct"
    | Nov ->
      "nov"
    | Dec ->
      "dec"
  ;;

  let from_int = function
    | 1 ->
      Ok Jan
    | 2 ->
      Ok Feb
    | 3 ->
      Ok Mar
    | 4 ->
      Ok Apr
    | 5 ->
      Ok May
    | 6 ->
      Ok Jun
    | 7 ->
      Ok Jul
    | 8 ->
      Ok Aug
    | 9 ->
      Ok Sep
    | 10 ->
      Ok Oct
    | 11 ->
      Ok Nov
    | 12 ->
      Ok Dec
    | n ->
      Error (Invalid_month n)
  ;;

  let to_char = function
    | Jan ->
      'a'
    | Feb ->
      'b'
    | Mar ->
      'c'
    | Apr ->
      'd'
    | May ->
      'e'
    | Jun ->
      'f'
    | Jul ->
      'g'
    | Aug ->
      'h'
    | Sep ->
      'i'
    | Oct ->
      'j'
    | Nov ->
      'k'
    | Dec ->
      'l'
  ;;

  let from_char char =
    let c = Char.lowercase_ascii char |> int_of_char in
    let a = int_of_char 'a' in
    from_int (c - a + 1)
  ;;
end
