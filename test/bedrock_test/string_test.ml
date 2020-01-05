open Bedrock
open Alcotest
open Test_tools

module Start_with = struct
  let suite =
    [ "with two empty strings", true, "", ""
    ; "with empty suffix", true, "foo", ""
    ; "with valid suffix 1", true, "foobar", "fo"
    ; "with valid suffix 2", true, "foobar", "foo"
    ; "with two equals strings", true, "foobar", "foobar"
    ; "with base as an empty string", false, "", "foobar"
    ; "with invalid suffix", false, "foobar", "bar"
    ]
    |> List.map (fun (message, result, left, right) ->
           test (Format.sprintf "[start_with] %s" message) (fun () ->
               check
                 bool
                 ("Should be " ^ string_of_bool result)
                 result
                 (String.start_with left right)))
  ;;
end

module End_with = struct
  let suite =
    [ "with two empty strings", true, "", ""
    ; "with empty suffix", true, "foo", ""
    ; "with valid suffix 1", true, "foobar", "r"
    ; "with valid suffix 2", true, "foobar", "bar"
    ; "with two equals strings", true, "foobar", "foobar"
    ; "with base as an empty string", false, "", "foobar"
    ; "with invalid suffix", false, "foobar", "foo"
    ]
    |> List.map (fun (message, result, left, right) ->
           test (Format.sprintf "[end_with] %s" message) (fun () ->
               check
                 bool
                 ("Should be " ^ string_of_bool result)
                 result
                 (String.end_with left right)))
  ;;
end

module Has_extension = struct
  let suite =
    [ "with valid suffix 1", true, "foobar.exe", "exe"
    ; "with valid suffix 2", true, "foobar.tar.gz", "tar.gz"
    ; "with two equals strings", true, ".foobar", "foobar"
    ; "with base as an empty string", false, "", ".exe"
    ; "with invalid suffix", false, "foobar.zip", "rar"
    ]
    |> List.map (fun (message, result, left, right) ->
           test (Format.sprintf "[has_extension] %s" message) (fun () ->
               check
                 bool
                 ("Should be " ^ string_of_bool result)
                 result
                 (String.has_extension left right)))
  ;;
end

module Super_trim = struct
  let suite =
    [ "foobar", "foobar"
    ; "foo bar", "foobar"
    ; "rgb(x, y, z)", "rgb(x,y,z)"
    ; "f o o\tb\ta r", "foobar"
    ]
    |> List.map (fun (base, expected) ->
           test (Format.sprintf "[super_trim] %s -> %s" base expected) (fun () ->
               let result = String.super_trim base in
               check string "same strings" expected result))
  ;;
end

let suite = Start_with.suite @ End_with.suite @ Has_extension.suite @ Super_trim.suite
