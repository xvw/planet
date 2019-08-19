open Bedrock
open Error
open Util

type git_repo =
  { username : string
  ; name : string
  }

type t =
  | Github of git_repo
  | Gitlab of git_repo

let t_to_string = function
  | Github _ ->
    "github"
  | Gitlab _ ->
    "gitlab"
;;

let ftrim = String.trim %> String.lowercase_ascii

let github ~user project_name =
  Github { username = ftrim user; name = ftrim project_name }
;;

let gitlab ~user project_name =
  Gitlab { username = ftrim user; name = ftrim project_name }
;;

let domain = function
  | Github _ ->
    "github.com"
  | Gitlab _ ->
    "gitlab.com"
;;

let scheme = function Github _ | Gitlab _ -> "https"

let repr = function
  | (Github { username; name } | Gitlab { username; name }) as repo
    ->
    let base = t_to_string repo in
    Format.asprintf "%s/%s/%s" base username name
;;

let base_url = function
  | (Github { username; name } | Gitlab { username; name }) as repo
    ->
    let protocol = scheme repo in
    let base = domain repo in
    Format.asprintf "%s://%s/%s/%s" protocol base username name
;;

let https_reference = function
  | (Github _ | Gitlab _) as repo ->
    let left = base_url repo in
    Format.asprintf "%s.git" left
;;

let ssh_reference = function
  | (Github { username; name } | Gitlab { username; name }) as repo
    ->
    let left = domain repo in
    Format.asprintf "git@%s:%s/%s.git" left username name
;;

let bucktracker_url = function
  | (Github _ | Gitlab _) as repo ->
    let left = base_url repo in
    Format.asprintf "%s/issues" left
;;

let releases_url = function
  | Gitlab _ as repo ->
    let left = base_url repo in
    Format.asprintf "%s/-/releases" left
  | Github _ as repo ->
    let left = base_url repo in
    Format.asprintf "%s/releases" left
;;

let to_qexp = function
  | (Github { username; name } | Gitlab { username; name }) as repo
    ->
    let open Paperwork.Qexp in
    let kind = t_to_string repo in
    node [ keyword kind; atom username; atom name ]
;;

let sid x = Ok x

let from_couple (repo, username, name) =
  match ftrim repo with
  | "github" ->
    Ok (github ~user:username name)
  | "gitlab" ->
    Ok (gitlab ~user:username name)
  | _ ->
    Error [ Of ("Unknown repo kind " ^ repo) ]
;;

let from_qexp expr =
  Paperwork.Table.Mapper.(
    triple $ token sid $ token sid $ token sid $ expr)
  |> Validation.bind from_couple
;;
