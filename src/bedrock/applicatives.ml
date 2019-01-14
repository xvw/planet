(* module List = Applicative.Make (struct
 *   type 'a t = 'a list
 * 
 *   let pure x = [x]
 *   let ap = ??
 * end)
 * 
 * module Option = Functor.Make (struct
 *   type 'a t = 'a option
 * 
 *   let pure x = Some x
 *   let map f = function None -> None | Some x -> Some (f x)
 * end)
 * 
 * module Array = Functor.Make (struct
 *   type 'a t = 'a array
 * 
 *   let pure x = [|x|]
 *   let map f x = Stdlib.Array.map f x
 * end)
 * 
 * module Result = Functor.Make (struct
 *   type 'a t = 'a Result.t
 * 
 *   let pure x = Ok x
 *   let map f = function Error x -> Error x | Ok x -> Ok (f x)
 * end) *)
