open Paperwork
open Timetable
open Test_tools
open Bedrock
open Bedrock.Util
open! Error
open Alcotest

let test_year_builder1 () =
  let open Result.Infix in
  match Year.make 18 >|= Year.to_string with
  | Ok "018" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 018")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder2 () =
  let open Result.Infix in
  match Year.make 0 >|= Year.to_string with
  | Ok "000" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 000")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder3 () =
  let open Result.Infix in
  match Year.make 999 >|= Year.to_string with
  | Ok "999" ->
    ()
  | Ok x ->
    failwith (x ^ " is not equals to 999")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder4 () =
  let open Result.Infix in
  match Year.make (-87) >|= Year.to_string with
  | Error (Invalid_year -87) ->
    ()
  | Ok x ->
    failwith (x ^ " shoud fail")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_year_builder5 () =
  let open Result.Infix in
  match Year.make 1000 >|= Year.to_string with
  | Error (Invalid_year 1000) ->
    ()
  | Ok x ->
    failwith (x ^ " shoud fail")
  | Error err ->
    failwith (Error.to_string err)
;;

let test_month_builder1 () =
  let open Result.Infix in
  Year.make 18
  >>= (fun y -> Month.(make y Jan))
  >|= Month.to_string
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
  Year.make 216
  >>= (fun y -> Month.(make y Nov))
  >|= Month.to_string
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
  Year.make 1000
  >>= (fun y -> Month.(make y Nov))
  >|= Month.to_string
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
        match Year.make x >|= Year.is_leap with
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
  Year.make 19
  >>= (fun x -> Month.(make x Mar))
  >>= (fun m -> Day.make m 12)
  >|= Day.to_string
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
  Year.make 222
  >>= (fun x -> Month.(make x Oct))
  >>= (fun m -> Day.make m 31)
  >|= Day.to_string
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
  Year.make 4
  >>= (fun x -> Month.(make x Feb))
  >>= (fun m -> Day.make m 29)
  >|= Day.to_string
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
  Year.make 19
  >>= (fun x -> Month.(make x Apr))
  >>= (fun m -> Day.make m 31)
  >|= Day.to_string
  |> function
  | Error (Invalid_day 31) -> () | _ -> failwith "Sould not be valid"
;;

let test_day_builder5 () =
  let open Result.Infix in
  Year.make 19
  >>= (fun x -> Month.(make x Apr))
  >>= (fun m -> Day.make m (-31))
  >|= Day.to_string
  |> function
  | Error (Invalid_day -31) ->
    ()
  | _ ->
    failwith "Sould not be valid"
;;

let test_day_with1 () =
  let open Result.Infix in
  match Day.make_with 18 Month.May 31 >|= Day.to_string with
  | Ok "018E31" ->
    ()
  | _ ->
    failwith "should be valid"
;;

let test_day_with2 () =
  let open Result.Infix in
  match Day.make_with 4 Month.Feb 29 >|= Day.to_string with
  | Ok "004B29" ->
    ()
  | _ ->
    failwith "should be valid"
;;

let test_day_with3 () =
  let open Result.Infix in
  match Day.make_with 3 Month.Feb 29 >|= Day.to_string with
  | Error (Invalid_day 29) ->
    ()
  | _ ->
    failwith "should be invalid"
;;

let test_day_with4 () =
  let open Result.Infix in
  match Day.make_with 1929 Month.Feb 1 >|= Day.to_string with
  | Error (Invalid_year 1929) ->
    ()
  | _ ->
    failwith "should be invalid"
;;

let test_hour1 () =
  let open Result.Infix in
  match Hour.make 22 59 >|= Hour.to_string with
  | Ok "10PM59" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_hour2 () =
  let open Result.Infix in
  match Hour.make 7 35 >|= Hour.to_string with
  | Ok "07AM35" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_hour3 () =
  let open Result.Infix in
  match Hour.make 12 12 >|= Hour.to_string with
  | Ok "12PM12" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_hour4 () =
  let open Result.Infix in
  match Hour.make 0 5 >|= Hour.to_string with
  | Ok "12AM05" ->
    ()
  | Ok x ->
    failwith x
  | _ ->
    failwith "should be valid"
;;

let test_moment_with1 () =
  let open Result.Infix in
  match
    Moment.make_with 4 Month.Feb 29 0 45 >|= Moment.to_string
  with
  | Ok "004B29:12AM45" ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with2 () =
  let open Result.Infix in
  match
    Moment.make_with 19 Month.Nov 3 21 58 >|= Moment.to_string
  with
  | Ok "019K03:09PM58" ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with3 () =
  let open Result.Infix in
  match
    Moment.make_with (-6) Month.Nov 3 21 58 >|= Moment.to_string
  with
  | Error (Invalid_year -6) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with4 () =
  let open Result.Infix in
  match
    Moment.make_with 6 Month.Feb 29 21 58 >|= Moment.to_string
  with
  | Error (Invalid_day 29) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with5 () =
  let open Result.Infix in
  match
    Moment.make_with 4 Month.Feb 29 (-12) 58 >|= Moment.to_string
  with
  | Error (Invalid_hour -12) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_moment_with6 () =
  let open Result.Infix in
  match
    Moment.make_with 4 Month.Feb 29 5 61 >|= Moment.to_string
  with
  | Error (Invalid_min 61) ->
    ()
  | Ok x ->
    failwith ("ok: " ^ x)
  | Error err ->
    failwith ("error: " ^ Error.to_string err)
;;

let test_year_from_string1 () =
  let open Result.Infix in
  let subject = ["000"; "001"; "123"; "156"; "999"] in
  let output =
    List.bind
      (fun x ->
        Year.from_string x >|= Year.to_string
        |> function Ok x -> [x] | _ -> [] )
      subject
  in
  check (list string) "same list" subject output
;;

let test_year_from_string2 () =
  let open Result.Infix in
  let subject = [""; "foo"; "-123"; "190"; "1560"; "bar"] in
  let output =
    List.bind
      (fun x ->
        Year.from_string x >|= Year.to_string
        |> function Error x -> [Error.to_string x] | _ -> [] )
      subject
  in
  check
    (list string)
    "same list"
    [ Error.to_string $ Unparsable ""
    ; Error.to_string $ Unparsable "foo"
    ; Error.to_string $ Unparsable "-123"
    ; Error.to_string $ Unparsable "1560"
    ; Error.to_string $ Unparsable "bar" ]
    output
;;

let test_month_from_string1 () =
  let open Result.Infix in
  let subject = ["000A"; "001D"; "123C"; "156E"; "999F"] in
  let output =
    List.bind
      (fun x ->
        Month.from_string x >|= Month.to_string
        |> function Ok x -> [x] | _ -> [] )
      subject
  in
  check (list string) "same list" subject output
;;

let test_month_from_string2 () =
  let open Result.Infix in
  let subject = [""; "foo"; "-123"; "190R"; "1"; "210Z"] in
  let output =
    List.bind
      (fun x ->
        Month.from_string x >|= Month.to_string
        |> function Error x -> [Error.to_string x] | _ -> [] )
      subject
  in
  check
    (list string)
    "same list"
    [ Error.to_string $ Unparsable ""
    ; Error.to_string $ Unparsable "foo"
    ; Error.to_string $ Invalid_year (-12)
    ; Error.to_string $ Invalid_month 18
    ; Error.to_string $ Unparsable "1"
    ; Error.to_string $ Invalid_month 26 ]
    output
;;

let suite =
  [ test "[year] Build a valid year 1" test_year_builder1
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
  ; test "[Year.from_string] Valid strings 1" test_year_from_string1
  ; test
      "[Year.from_string] Invalid strings 1"
      test_year_from_string2
  ; test
      "[Month.from_string] Valid strings 1"
      test_month_from_string1
  ; test
      "[Month.from_string] Invalid strings 2"
      test_month_from_string2 ]
;;
