open Bedrock
open Error
open Util

type name = string
type line = string
type chmod = int
type extension = string
type old_name = string
type new_name = string

let exists = Sys.file_exists

let is_directory name =
  if exists name then Ok (Sys.is_directory name) else Error (Unreadable name)
;;

let bytes_of_in_channel channel =
  let length = in_channel_length channel in
  let buffer = Bytes.create length in
  let () = really_input channel buffer 0 length in
  buffer
;;

let in_channel filename =
  try
    let channel = open_in filename in
    Ok channel
  with
  | _ -> Error (Unreadable filename)
;;

let close_in = Stdlib.close_in_noerr

let read f filename =
  let open Result.Infix in
  filename
  |> in_channel
  >|= (fun x -> x, f x)
  >>= fun (channel, result) ->
  let () = close_in channel in
  result
;;

let to_stream f filename =
  try
    let channel = open_in filename in
    let stream = Stream.of_channel channel in
    let result = f filename stream in
    let () = close_in channel in
    result
  with
  | _ -> Error (Unreadable filename)
;;

let to_bytes filename =
  try
    let channel = open_in filename in
    let bytes = bytes_of_in_channel channel in
    let () = close_in channel in
    Ok bytes
  with
  | _ -> Error (Unreadable filename)
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
      with
      | End_of_file -> Ok (List.rev acc)
    in
    loop []
  with
  | _ -> Error (Unreadable filename)
;;

let lines filename =
  try
    let channel = open_in filename in
    let rec loop acc =
      try
        let line = input_line channel in
        loop $ line :: acc
      with
      | End_of_file -> Ok (List.rev acc)
    in
    loop []
  with
  | _ -> Error (Unreadable filename)
;;

let out_channel
    ?(flags = [ Open_wronly; Open_creat ])
    ?(binary = false)
    ?(append = false)
    ?(chmod = 0o777)
    ?(overwrite = false)
    filename
  =
  let mode = if binary then Open_binary else Open_text in
  let appd = if append then Open_append else Open_trunc in
  let over = if overwrite then [] else [ Open_excl ] in
  let f = mode :: appd :: (flags @ over) in
  try Ok (open_out_gen f chmod filename) with
  | Sys_error str when str = filename ^ ": File exists" ->
    Error (Already_exists filename)
  | _ -> Error (Unreadable filename)
;;

let close_out = Stdlib.close_out_noerr

let write
    ?(flags = [ Open_wronly; Open_creat ])
    ?(binary = false)
    ?(append = false)
    ?(chmod = 0o777)
    ?(overwrite = false)
    f
    filename
  =
  let open Result.Infix in
  filename
  |> out_channel ~flags ~binary ~append ~chmod ~overwrite
  >|= (fun x -> x, f x)
  >>= fun (channel, result) ->
  let () = close_out channel in
  result
;;

let create ?(binary = false) ?(chmod = 0o777) filename content =
  write
    ~chmod
    ~binary
    (fun channel -> Ok (output_string channel content))
    filename
;;

let touch ?(binary = false) ?(chmod = 0o777) filename =
  if exists filename then Ok () else create ~binary ~chmod filename ""
;;

let append ?(binary = false) ?(create = false) ?(chmod = 0o777) filename content
  =
  if (not create) && not (exists filename)
  then Error (Unreadable filename)
  else
    write
      ~binary
      ~chmod
      ~overwrite:true
      ~append:true
      (fun channel -> Ok (output_string channel content))
      filename
;;

let overwrite
    ?(binary = false)
    ?(create = false)
    ?(chmod = 0o777)
    filename
    content
  =
  if (not create) && not (exists filename)
  then Error (Unreadable filename)
  else
    write
      ~binary
      ~chmod
      ~overwrite:true
      ~append:false
      (fun channel -> Ok (output_string channel content))
      filename
;;

let delete filename =
  try Ok (Sys.remove filename) with
  | _ -> Error (Unreadable filename)
;;

let rename old_name new_name =
  try Ok (Sys.rename old_name new_name) with
  | _ -> Error (Unreadable (old_name ^ " -> " ^ new_name))
;;
