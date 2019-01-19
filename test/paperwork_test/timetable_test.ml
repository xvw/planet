open Paperwork
open Timetable
open Test_tools
open Bedrock
open! Error

let test_from_int1 () =
  match
    List.map Month.from_int [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]
  with
  | Month.([ Ok Jan
           ; Ok Feb
           ; Ok Mar
           ; Ok Apr
           ; Ok May
           ; Ok Jun
           ; Ok Jul
           ; Ok Aug
           ; Ok Sep
           ; Ok Oct
           ; Ok Nov
           ; Ok Dec ]) ->
    ()
  | xs ->
    List.iter
      (function Error x -> failwith (Error.to_string x) | _ -> ())
      xs
;;

let test_from_int2 () =
  match List.map Month.from_int [-3; 0; 13; 14; -789] with
  | [ Error (Invalid_month -3)
    ; Error (Invalid_month 0)
    ; Error (Invalid_month 13)
    ; Error (Invalid_month 14)
    ; Error (Invalid_month -789) ] ->
    ()
  | _ ->
    failwith "It should fail"
;;

let test_year_builder1 () =
  let open Result.Infix in
  match year 18 >|= year_to_string with
  | Ok "018" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 018")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder2 () =
  let open Result.Infix in
  match year 0 >|= year_to_string with
  | Ok "000" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 000")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder3 () =
  let open Result.Infix in
  match year 999 >|= year_to_string with
  | Ok "999" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 999")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder4 () =
  let open Result.Infix in
  match year (-87) >|= year_to_string with
  | Error (Invalid_year -87) ->
    ()
  | Ok x ->
    failwith (x ^ " shoud fail")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder5 () =
  let open Result.Infix in
  match year 1000 >|= year_to_string with
  | Error (Invalid_year 1000) ->
    ()
  | Ok x ->
    failwith (x ^ " shoud fail")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_month_builder1 () =
  let open Result.Infix in
  year 18
  >>= (fun y -> month y Jan)
  >|= month_to_string
  |> function
  | Ok "018A" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 018A")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_month_builder2 () =
  let open Result.Infix in
  year 216
  >>= (fun y -> month y Nov)
  >|= month_to_string
  |> function
  | Ok "216K" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 216K")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_month_builder3 () =
  let open Result.Infix in
  year 1000
  >>= (fun y -> month y Nov)
  >|= month_to_string
  |> function
  | Error (Invalid_year 1000) ->
    ()
  | Ok x ->
    failwith (x ^ " should not be valid")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_is_leap () =
  let open Result.Infix in
  match
    List.filter
      (fun x ->
        match year x >|= is_leap with
        | Ok bool ->
          bool
        | Error err ->
          failwith
            ("Invalid year generation : " ^ Error.to_string err) )
      [1; 2; 4; 72; 96; 952; 997; 24]
  with
  | [4; 72; 96; 952; 24] ->
    ()
  | _ ->
    failwith "Invalid result"
;;

let test_day_builder1 () =
  let open Result.Infix in
  year 19
  >>= (fun x -> month x Mar)
  >>= (fun m -> day m 12)
  >|= day_to_string
  |> function
  | Ok "019C12" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 019C12")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_day_builder2 () =
  let open Result.Infix in
  year 222
  >>= (fun x -> month x Oct)
  >>= (fun m -> day m 31)
  >|= day_to_string
  |> function
  | Ok "222J31" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 222J31")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_day_builder3 () =
  let open Result.Infix in
  year 4
  >>= (fun x -> month x Feb)
  >>= (fun m -> day m 29)
  >|= day_to_string
  |> function
  | Ok "004B29" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 004B29")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_day_builder4 () =
  let open Result.Infix in
  year 19
  >>= (fun x -> month x Apr)
  >>= (fun m -> day m 31)
  >|= day_to_string
  |> function
  | Error (Invalid_day 31) -> () | _ -> failwith "Sould not be valid"
;;

let test_day_builder5 () =
  let open Result.Infix in
  year 19
  >>= (fun x -> month x Apr)
  >>= (fun m -> day m (-31))
  >|= day_to_string
  |> function
  | Error (Invalid_day -31) ->
    ()
  | _ ->
    failwith "Sould not be valid"
;;

let test_day_with1 () =
  let open Result.Infix in
  match day_with 18 May 31 >|= day_to_string with
  | Ok "018E31" ->
    ()
  | _ ->
    failwith "should be valid"
;;

let test_day_with2 () =
  let open Result.Infix in
  match day_with 4 Feb 29 >|= day_to_string with
  | Ok "004B29" ->
    ()
  | _ ->
    failwith "should be valid"
;;

let test_day_with3 () =
  let open Result.Infix in
  match day_with 3 Feb 29 >|= day_to_string with
  | Error (Invalid_day 29) ->
    ()
  | _ ->
    failwith "should be invalid"
;;

let test_day_with4 () =
  let open Result.Infix in
  match day_with 1929 Feb 1 >|= day_to_string with
  | Error (Invalid_year 1929) ->
    ()
  | _ ->
    failwith "should be invalid"
;;

let test_hour1 () =
  let open Result.Infix in
  match hour 22 59 >|= hour_to_string with
  | Ok "10PM59" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_hour2 () =
  let open Result.Infix in
  match hour 7 35 >|= hour_to_string with
  | Ok "07AM35" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_hour3 () =
  let open Result.Infix in
  match hour 12 12 >|= hour_to_string with
  | Ok "12PM12" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_hour4 () =
  let open Result.Infix in
  match hour 0 5 >|= hour_to_string with
  | Ok "12AM05" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_moment_with1 () =
  let open Result.Infix in
  match moment_with 4 Feb 29 0 45 >|= moment_to_string with
  | Ok "004B29:12AM45" ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with2 () =
  let open Result.Infix in
  match moment_with 19 Nov 3 21 58 >|= moment_to_string with
  | Ok "019K03:09PM58" ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with3 () =
  let open Result.Infix in
  match moment_with (-6) Nov 3 21 58 >|= moment_to_string with
  | Error (Invalid_year -6) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with4 () =
  let open Result.Infix in
  match moment_with 6 Feb 29 21 58 >|= moment_to_string with
  | Error (Invalid_day 29) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with5 () =
  let open Result.Infix in
  match moment_with 4 Feb 29 (-12) 58 >|= moment_to_string with
  | Error (Invalid_hour -12) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with6 () =
  let open Result.Infix in
  match moment_with 4 Feb 29 5 61 >|= moment_to_string with
  | Error (Invalid_min 61) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let suite =
  [ test "[Month.from_int] Test in trivial case" test_from_int1
  ; test "[Month.from_int] Test in failure case" test_from_int2
  ; test "[year] Build a valid year 1" test_year_builder1
  ; test "[year] Build a valid year 2" test_year_builder2
  ; test "[year] Build a valid year 3" test_year_builder3
  ; test "[year] Build an invalid year 1" test_year_builder4
  ; test "[year] Build an invalid year 2" test_year_builder5
  ; test "[month] Build a valid month 1" test_month_builder1
  ; test "[month] Build a valid month 2" test_month_builder2
  ; test "[month] Build an invalid month 1" test_month_builder3
  ; test "[is_leap] check is_leap with some values" test_is_leap
  ; test "[day] Build a valid day 1" test_day_builder1
  ; test "[day] Build a valid day 2" test_day_builder2
  ; test "[day] Build a valid day 3 (in leap year)" test_day_builder3
  ; test "[day] Build an invalid day 1" test_day_builder4
  ; test "[day] Build an invalid day 2" test_day_builder5
  ; test "[day_with] Build a valid day 1" test_day_with1
  ; test "[day_with] Build a valid day 2" test_day_with2
  ; test "[day_with] Build an invalid day 1" test_day_with3
  ; test "[day_with] Build a valid day 4" test_day_with4
  ; test "[hour] Build a valid hour 1" test_hour1
  ; test "[hour] Build a valid hour 2" test_hour2
  ; test "[hour] Build a valid hour 3" test_hour3
  ; test "[hour] Build a valid hour 4" test_hour4
  ; test "[moment_with] Build a valid moment 1" test_moment_with1
  ; test "[moment_with] Build a valid moment 2" test_moment_with2
  ; test "[moment_with] Build an invalid moment 1" test_moment_with3
  ; test "[moment_with] Build an invalid moment 2" test_moment_with4
  ; test "[moment_with] Build an invalid moment 3" test_moment_with5
  ; test "[moment_with] Build an invalid moment 4" test_moment_with6
  ]
;;
