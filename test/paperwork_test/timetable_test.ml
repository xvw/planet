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
  ; test "[day] Build an invalid 1" test_day_builder4
  ; test "[day] Build an invalid 2" test_day_builder5 ]
;;
