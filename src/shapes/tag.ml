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

let content_to_qexp content =
  let open Qexp in
  node
    [ kv "title" content.title
    ; kv "section" content.section
    ; kv "id" content.id
    ; kv "date" (Timetable.Day.to_string content.date)
    ; node [ tag "tags"; node (List.map string content.tags) ]
    ]
;;

let content_to_json content =
  let open Json in
  obj
    [ ("title", string content.title)
    ; ("section", string content.section)
    ; ("id", string content.id)
    ; ("date", string (Timetable.Day.to_string content.date))
    ; ("tags", array (List.map string content.tags))
    ]
;;

let to_qexp bucket =
  let open Qexp in
  node
    [ node [ tag "tags"; node (List.map string bucket.tags) ]
    ; node [ tag "contents"; node (List.map content_to_qexp bucket.contents) ]
    ]
;;

let to_json bucket =
  let open Json in
  obj
    [ ("tags", array (List.map string bucket.tags))
    ; ("contents", array (List.map content_to_json bucket.contents))
    ]
;;

let sort bucket =
  { tags = List.sort_uniq String.compare bucket.tags
  ; contents =
      List.sort (fun x y -> Timetable.Day.cmp y.date x.date) bucket.contents
  }
;;

let add bucket title section id date tags =
  { tags = List.append bucket.tags tags
  ; contents = { title; section; id; date; tags } :: bucket.contents
  }
;;
