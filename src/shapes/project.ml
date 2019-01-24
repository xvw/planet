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
