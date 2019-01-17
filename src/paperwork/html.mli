(** Describe [Html] using [Qexp] : 

{[
(head ((title ("Document example"))))
(body
  ((div {(:id "app") (:class "--main-content")}
     ((h1 ("Hello World")) (p {(:main-paragraph)} ("Lorem ipsum..."))))))
]}
 *)

open Bedrock

(** {2 Types} *)

(** [Html] attribute *)
type attr =
  | Flag of string  (** Simple attribute like [checked] *)
  | Pair of string * string  (** Pair attribute [x="y"] *)

(** [Html] node *)
type node =
  | Leaf of attr list  (** Leaf node like [<br ..>] *)
  | Node of (attr list * node list)
      (** Normal node like [<div ..></div>]*)

(** {2 Parsing} *)

val process_attributes : Qexp.t -> attr list Result.t

(** Produce [Html] from [Qexp] *)
(* val from_qexp : Qexp.t -> node list Bedrock.Result.t *)
