open Bedrock
open Error
open Util

type name = string
type line = string
type chmod = int
type extension = string

let bytes_of_in_channel channel =
  let length = in_channel_length channel in
  let buffer = Bytes.create length in
  let () = really_input channel buffer 0 length in
  buffer
;;

let to_stream filename =
  try
    let channel = open_in filename in
    let stream = Stream.of_channel channel in
    let () = close_in channel in
    Ok stream
  with _ -> Error (Unreadable filename)
;;

let to_bytes filename =
  try
    let channel = open_in filename in
    let bytes = bytes_of_in_channel channel in
    let () = close_in channel in
    Ok bytes
  with _ -> Error (Unreadable filename)
;;

let to_string filename =
  let open Result.Infix in
  filename |> to_bytes >|= Bytes.to_string
;;

let chars filename =
  try
    let channel = open_in filename in
    let rec loop acc =
      try
        let char = input_char channel in
        loop $ char :: acc
      with End_of_file -> Ok (List.rev acc)
    in
    loop []
  with _ -> Error (Unreadable filename)
;;

let lines filename =
  try
    let channel = open_in filename in
    let rec loop acc =
      try
        let line = input_line channel in
        loop $ line :: acc
      with End_of_file -> Ok (List.rev acc)
    in
    loop []
  with _ -> Error (Unreadable filename)
;;
