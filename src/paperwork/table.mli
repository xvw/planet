(** [Hashtbl.t] 's specialization. (To deal with [Qexp.t]) *)

open Bedrock

(** {2 Types} *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t
type configuration = (string, Qexp.t option) t

(**{2 Builder} *)

(** Build a [Table.t] from a [Qexp.t] *)
val from_qexp_gen :
     (Qexp.t -> Qexp.t list -> ('a * 'b) Result.t)
  -> Qexp.t
  -> ('a, 'b) t Result.t

(** Build a [Table.t] from a [Qexp.t] with k/v [Qexp.t]. *)
val from_qexp :
     (string -> Qexp.t list -> ('a * 'b) Result.t)
  -> Qexp.t
  -> ('a, 'b) t Result.t

(** Build a [Table.configuration] from a [Qexp.t]. *)
val configuration : Qexp.t -> configuration Result.t

(let fetch_string_opt table field continuation =
  match Hashtbl.find_opt table field with
  | Some (Some (Qexp.String (_, k))) ->
    Ok (continuation $ Some k)
  | None ->
    Ok (continuation None)
  | _ ->
    Error [Invalid_field field]
;;
** {2 Fetch data from table} 
    
    A [Table.t] is fetchable to extract fields using predicates.
*)

type ('a, 'b) fetchable =
  configuration -> string -> ('a -> 'b) -> 'b Validation.t

val fetch_string : (string, 'a) fetchable
val fetch_string_opt : (string option, 'a) fetchable
val fetch_bool : (bool, 'a) fetchable
val fetch_bool_opt : (bool option, 'a) fetchable
