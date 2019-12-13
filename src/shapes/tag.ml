open Paperwork

type t = string

type content =
  { title : string
  ; section : string
  ; id : string
  ; date : Paperwork.Timetable.Day.t
  ; tags : t list
  }

type bucket =
  { tags : t list
  ; contents : content list
  }

let to_qexp bucket =
  let open Qexp in
  node
    [ node [ tag "tags"; node (List.map string bucket.tags) ]
    ; node [ tag "contents"; node [] ]
    ]
;;
