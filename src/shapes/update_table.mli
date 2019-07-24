(** Store updates into a flat key-value engine *)

open Bedrock
open Paperwork

(** {2 Types} *)

type t

(** {2 Coercion} *)

val from_qexp : Qexp.t -> t Result.t
val to_qexp : t -> Qexp.t
