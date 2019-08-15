module Projects = struct
  type context =
    { name : string
    ; start_date : Paperwork.Timetable.Day.t
    ; logs_counter : int
    ; minuts_counter : int
    ; sectors_counters : (string, int) Hashtbl.t
    }

  type t =
    { updates : Shapes.Update_table.t
    ; projects : (string, context) Hashtbl.t
    }

  let add_to_sectors ctx sector =
    match Hashtbl.find_opt ctx sector with
    | None ->
      let () = Hashtbl.add ctx sector 1 in
      ctx
    | Some i ->
      let () = Hashtbl.remove ctx sector in
      let () = Hashtbl.add ctx sector (i + 1) in
      ctx
  ;;

  let init_project project log =
    let open Shapes.Log in
    let sectors = add_to_sectors (Hashtbl.create 1) log.sector in
    { name = project
    ; start_date = log.day
    ; logs_counter = 1
    ; minuts_counter = log.duration
    ; sectors_counters = sectors
    }
  ;;

  let keep_smallest_date date1 date2 =
    if Paperwork.Timetable.Day.cmp date1 date2 < 0
    then date2
    else date1
  ;;

  let diff base log =
    let open Shapes.Log in
    { base with
      start_date = keep_smallest_date base.start_date log.day
    ; logs_counter = base.logs_counter + 1
    ; minuts_counter = base.minuts_counter + log.duration
    ; sectors_counters =
        add_to_sectors base.sectors_counters log.sector
    }
  ;;

  let update_project ctx project log =
    match Hashtbl.find_opt ctx project with
    | None ->
      let () = Hashtbl.add ctx project (init_project project log) in
      ctx
    | Some summary ->
      let () = Hashtbl.remove ctx project in
      let () = Hashtbl.add ctx project (diff summary log) in
      ctx
  ;;

  let update ctx log =
    let open Shapes.Log in
    match log.project with
    | None ->
      ctx
    | Some project ->
      { ctx with projects = update_project ctx.projects project log }
  ;;

  let init table = { updates = table; projects = Hashtbl.create 1 }

  let aux_project_qexp name value =
    let open Paperwork.Qexp in
    let sectors =
      value.sectors_counters |> Hashtbl.to_seq |> List.of_seq
      |> List.map (fun (k, value) ->
             node [ string k; atom (string_of_int value) ])
    in
    node
      [ string name
      ; node
          [ node [ tag "name"; string value.name ]
          ; node
              [ tag "start_date"
              ; keyword
                  (Paperwork.Timetable.Day.to_string value.start_date)
              ]
          ; node
              [ tag "logs_counter"
              ; atom (string_of_int value.logs_counter)
              ]
          ; node
              [ tag "minuts_counter"
              ; atom (string_of_int value.minuts_counter)
              ]
          ; node [ tag "sectors_counters"; node sectors ]
          ]
      ]
  ;;

  let to_qexp ctx =
    let open Paperwork.Qexp in
    node
      [ node
          [ tag "updates"; Shapes.Update_table.to_qexp ctx.updates ]
      ; node
          [ tag "projects"
          ; node
              (ctx.projects |> Hashtbl.to_seq |> List.of_seq
              |> List.map (fun (k, v) -> aux_project_qexp k v))
          ]
      ]
  ;;

  let project_to_qexp ctx project =
    let open Bedrock.Option.Infix in
    Hashtbl.find_opt ctx.projects project
    >|= aux_project_qexp project
  ;;
end
