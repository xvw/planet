open Paperwork

type t = string

type content =
  { title : string
  ; section : string
  ; id : string
  ; date : Paperwork.Timetable.Day.t
  ; description : string
  ; tags : t list
  }

type bucket =
  { all_tags : t list
  ; contents : content list
  }

let new_bucket () = { all_tags = []; contents = [] }

let content_to_qexp content =
  let open Qexp in
  node
    [ kv "title" content.title
    ; kv "section" content.section
    ; kv "id" content.id
    ; kv "date" (Timetable.Day.to_string content.date)
    ; kv "description" content.description
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
    ; ("description", string content.description)
    ; ("tags", array (List.map string content.tags))
    ]
;;

let to_qexp bucket =
  let open Qexp in
  node
    [ node [ tag "all_tags"; node (List.map string bucket.all_tags) ]
    ; node [ tag "contents"; node (List.map content_to_qexp bucket.contents) ]
    ]
;;

let to_json bucket =
  let open Json in
  obj
    [ ("allTags", array (List.map string bucket.all_tags))
    ; ("contents", array (List.map content_to_json bucket.contents))
    ]
;;

let sort bucket =
  { all_tags = List.sort_uniq String.compare bucket.all_tags
  ; contents =
      List.sort (fun x y -> Timetable.Day.cmp y.date x.date) bucket.contents
  }
;;

let add bucket title section id date desc tags =
  let t = List.map String.lowercase_ascii tags in
  { all_tags = List.append bucket.all_tags t
  ; contents =
      { title; section; id; date; description = desc; tags = t }
      :: bucket.contents
  }
;;
