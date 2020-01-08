open Bedrock
open Util
open Paperwork
open Error
module Fetch = Table.Fetch
module Mapper = Table.Mapper

type state =
  | Backlog
  | Opened
  | InProgress
  | Done
  | Blocked

type t =
  { state : state
  ; uuid : string
  ; project : string option
  ; sectors : string list
  ; name : string
  ; description : string
  ; checklist : (bool * string) list
  ; tags : string list
  ; date : Paperwork.Timetable.Day.t
  ; opening_date : Paperwork.Timetable.Day.t option
  ; closing_date : Paperwork.Timetable.Day.t option
  ; engagement_date : Paperwork.Timetable.Day.t option
  }

type board =
  { backlog : int * t list
  ; opened : int * t list
  ; in_progress : int * t list
  ; done_ : int * t list
  ; blocked : int * t list
  }

let state_to_string = function
  | Backlog -> "backlog"
  | Opened -> "opened"
  | InProgress -> "in_progress"
  | Done -> "done"
  | Blocked -> "blocked"
;;

let state_from_string str =
  match String.lowercase_ascii str with
  | "backlog" -> Ok Backlog
  | "opened" -> Ok Opened
  | "in_progress" -> Ok InProgress
  | "done" -> Ok Done
  | "blocked" -> Ok Blocked
  | _ -> Error [ Of (Format.asprintf "Unknown state [%s]" str) ]
;;

let project_to_qexp = function
  | None -> []
  | Some x -> [ Qexp.kv "project" x ]
;;

let sectors_to_qexp sectors =
  Qexp.[ node [ tag "sectors"; node $ List.map atom sectors ] ]
;;

let checklist_to_qexp checks =
  Qexp.
    [ node
        [ tag "checklist"
        ; node
          $ List.map
              (fun (flag, label) ->
                node
                  [ (tag $ if flag then "checked" else "unchecked")
                  ; string label
                  ])
              checks
        ]
    ]
;;

let tags_to_qexp tags =
  Qexp.[ node [ tag "tags"; node $ List.map string tags ] ]
;;

let refutable_date_to_qexp key = function
  | None -> []
  | Some date -> Qexp.[ kv key $ Timetable.Day.to_string date ]
;;

let to_qexp task =
  let open Qexp in
  node
    ([ kv "name" task.name
     ; kv "description" task.description
     ; kv "state" $ state_to_string task.state
     ; kv "uuid" task.uuid
     ; kv "date" $ Timetable.Day.to_string task.date
     ]
    @ project_to_qexp task.project
    @ sectors_to_qexp task.sectors
    @ checklist_to_qexp task.checklist
    @ tags_to_qexp task.tags
    @ refutable_date_to_qexp "opening_date" task.opening_date
    @ refutable_date_to_qexp "closing_date" task.closing_date
    @ refutable_date_to_qexp "engagement_date" task.engagement_date)
;;

let new_task
    state
    uuid
    project
    sectors
    name
    description
    checklist
    tags
    date
    opening_date
    closing_date
    engagement_date
  =
  { state
  ; uuid
  ; project
  ; sectors
  ; name
  ; description
  ; checklist
  ; tags
  ; date
  ; opening_date
  ; closing_date
  ; engagement_date
  }
;;

let from_qexp expr =
  match Table.configuration expr with
  | Ok config ->
    let open Validation.Infix in
    new_task
    <$> Fetch.token state_from_string config "state"
    <*> Fetch.string config "uuid"
    <*> Fetch.(option string config "project")
    <*> Fetch.list (Mapper.token (fun x -> Ok x)) config "sectors"
    <*> Fetch.string config "name"
    <*> Fetch.string config "description"
    <*> Fetch.list_refutable
          (Mapper.couple
             (Mapper.token (fun flag ->
                  Ok (String.lowercase_ascii flag = "checked")))
             Mapper.string)
          config
          "checklist"
    <*> Fetch.list_refutable Mapper.string config "tags"
    <*> Fetch.day config "date"
    <*> Fetch.(option day config "opening_date")
    <*> Fetch.(option day config "closing_date")
    <*> Fetch.(option day config "engagement_date")
  | Error _ as e -> Validation.from_result e
;;

let to_json task =
  let dts = Timetable.Day.to_string in
  let open Json in
  obj
    [ "state", string $ state_to_string task.state
    ; "uuid", string task.uuid
    ; "project", nullable Option.(task.project >|= string)
    ; "sectors", array $ List.map string task.sectors
    ; "name", string task.name
    ; "description", string task.description
    ; ( "checklist"
      , array
        $ List.map
            (fun (flag, label) ->
              obj [ "checked", bool flag; "label", string label ])
            task.checklist )
    ; "tags", array $ List.map string task.tags
    ; "date", string $ dts task.date
    ; "openingDate", nullable Option.(task.opening_date >|= dts >|= string)
    ; "closingDate", nullable Option.(task.closing_date >|= dts >|= string)
    ; ( "engagementDate"
      , nullable Option.(task.engagement_date >|= dts >|= string) )
    ]
;;

let pp ppf task =
  let qexp = to_qexp task in
  Format.fprintf ppf "%a" Qexp.pp qexp
;;

let eq_state a b =
  match a, b with
  | Backlog, Backlog
  | Opened, Opened
  | InProgress, InProgress
  | Done, Done
  | Blocked, Blocked -> true
  | _ -> false
;;

let eq left right =
  eq_state left.state right.state
  && String.equal left.uuid right.uuid
  && Option.eq String.equal left.project right.project
  && List.eq String.equal left.sectors right.sectors
  && String.equal left.name right.name
  && String.equal left.description right.description
  && List.eq
       (fun (flag_a, label_a) (flag_b, label_b) ->
         flag_a = flag_b && String.equal label_a label_b)
       left.checklist
       right.checklist
  && List.eq String.equal left.tags right.tags
  && Timetable.Day.eq left.date right.date
  && Option.eq Timetable.Day.eq left.opening_date right.opening_date
  && Option.eq Timetable.Day.eq left.closing_date right.closing_date
  && Option.eq Timetable.Day.eq left.engagement_date right.engagement_date
;;

let smart_date_for task =
  let open Option in
  match task.state with
  | Done -> task.closing_date <!> fun () -> task.date
  | Opened | InProgress ->
    task.engagement_date <?> task.opening_date <!> fun () -> task.date
  | _ -> task.date
;;

let smart_sorter ?(desc = false) a b =
  let a_date = smart_date_for a in
  let b_date = smart_date_for b in
  let x, y = if desc then b_date, a_date else a_date, b_date in
  Paperwork.Timetable.Day.cmp x y
;;

let empty_board =
  { backlog = 0, []
  ; opened = 0, []
  ; in_progress = 0, []
  ; done_ = 0, []
  ; blocked = 0, []
  }
;;

let sort_board ?(desc = false) (i, l) = i, List.sort (smart_sorter ~desc) l

let board_create tasks =
  let unsorted_board =
    List.fold_left
      (fun board task ->
        match task.state with
        | Backlog ->
          let i, xs = board.backlog in
          { board with backlog = succ i, task :: xs }
        | Opened ->
          let i, xs = board.opened in
          { board with opened = succ i, task :: xs }
        | InProgress ->
          let i, xs = board.in_progress in
          { board with in_progress = succ i, task :: xs }
        | Done ->
          let i, xs = board.done_ in
          { board with done_ = succ i, task :: xs }
        | Blocked ->
          let i, xs = board.blocked in
          { board with blocked = succ i, task :: xs })
      empty_board
      tasks
  in
  { backlog = sort_board unsorted_board.backlog
  ; opened = sort_board unsorted_board.opened
  ; in_progress = sort_board unsorted_board.in_progress
  ; done_ = sort_board ~desc:true unsorted_board.done_
  ; blocked = sort_board unsorted_board.blocked
  }
;;

let board_to_json board =
  let open Json in
  let mk name elt =
    let total, tasks = elt in
    name, obj [ "total", int total; "tasks", array $ List.map to_json tasks ]
  in
  obj
    [ mk "backlog" board.backlog
    ; mk "opened" board.opened
    ; mk "inProgress" board.in_progress
    ; mk "isDone" board.done_
    ; mk "blocked" board.blocked
    ]
;;

let new_board backlog opened in_progress done_ blocked =
  { backlog; opened; in_progress; done_; blocked }
;;

let all_checked task = List.for_all (fun (f, _) -> f) task.checklist
let all_unchecked task = List.for_all (fun (f, _) -> not f) task.checklist
let has_checked task = List.exists (fun (f, _) -> f) task.checklist

let need_opening_date task =
  if has_checked task
  then (
    match task.opening_date with
    | None -> true
    | Some _ -> false)
  else false
;;

let need_closing_date task =
  if all_checked task
  then (
    match task.closing_date with
    | None -> true
    | Some _ -> false)
  else false
;;

let need_state_changement day task =
  match task.state with
  | Backlog ->
    if all_checked task
    then (Some Done, Option.unless task.opening_date (Some day), Some day), true
    else if has_checked task
    then
      ( ( Some InProgress
        , Option.unless task.opening_date (Some day)
        , task.closing_date )
      , true )
    else (None, task.opening_date, task.closing_date), false
  | Opened ->
    if all_checked task
    then (Some Done, Option.unless task.opening_date (Some day), Some day), true
    else if all_unchecked task
    then (Some Backlog, None, None), true
    else if has_checked task
    then
      (Some InProgress, Option.unless task.opening_date (Some day), None), true
    else (None, task.opening_date, task.closing_date), false
  | InProgress ->
    if all_checked task
    then (Some Done, Option.unless task.opening_date (Some day), Some day), true
    else if all_unchecked task
    then (Some Opened, Option.unless task.opening_date (Some day), None), true
    else (None, task.opening_date, task.closing_date), false
  | Done ->
    if all_unchecked task
    then (Some Opened, Some day, None), true
    else if not (all_checked task)
    then
      (Some InProgress, Option.unless task.opening_date (Some day), None), true
    else (None, task.opening_date, task.closing_date), false
  | _ -> (None, task.opening_date, task.closing_date), false
;;
