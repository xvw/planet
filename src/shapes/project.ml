(* open Bedrock *)
(* open Paperwork *)

type status =
  | Unceasing
  | Wip
  | Done
  | Paused
  | Interrupted

type t =
  { name : string
  ; title : string
  ; synopsis : string
  ; repo : string option
  ; license : string option
  ; tools : Link.simple list
  ; links : Link.simple list
  ; releases : Link.dated list
  ; status : status
  ; tags : string list
  ; picto : string option
  ; indexed : bool
  ; content : Text.t option }

(* let accumulate
 *     name
 *     title
 *     synopsis
 *     repo
 *     license
 *     tools
 *     links
 *     releases
 *     status
 *     tags
 *     picto
 *     indexed
 *     content
 *     () =
 *   { name
 *   ; title
 *   ; synopsis
 *   ; repo
 *   ; license
 *   ; tools
 *   ; links
 *   ; releases
 *   ; status
 *   ; tags
 *   ; picto
 *   ; indexed
 *   ; content }
 * ;; *)

(* let from_qexp expr =
 *   match Table.configuration expr with
 *   | Ok x ->
 *     Ok x
 *   | Error _ as e ->
 *     Validation.from_result e
 * ;; *)
