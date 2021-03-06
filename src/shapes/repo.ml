open Bedrock
open Error
open Util
module Mapper = Paperwork.Table.Mapper

type git_repo =
  { username : string
  ; name : string
  }

type t =
  | Github of git_repo
  | Gitlab of git_repo * string

let t_to_string = function
  | Github _ -> "github"
  | Gitlab _ -> "gitlab"
;;

let ftrim = String.trim %> String.lowercase_ascii

let github user project_name =
  Github { username = ftrim user; name = ftrim project_name }
;;

let gitlab user branch project_name =
  Gitlab ({ username = ftrim user; name = ftrim project_name }, ftrim branch)
;;

let domain = function
  | Github _ -> "github.com"
  | Gitlab _ -> "gitlab.com"
;;

let scheme = function
  | Github _ | Gitlab _ -> "https"
;;

let repr = function
  | (Github { username; name } | Gitlab ({ username; name }, _)) as repo ->
    let base = t_to_string repo in
    Format.asprintf "%s/%s/%s" base username name
;;

let kind = t_to_string

let base_url = function
  | (Github { username; name } | Gitlab ({ username; name }, _)) as repo ->
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
  | (Github { username; name } | Gitlab ({ username; name }, _)) as repo ->
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

let contributors_url = function
  | Github _ as repo ->
    let left = base_url repo in
    Format.asprintf "%s/graphs/contributors" left
  | Gitlab (_, branch) as repo ->
    let left = base_url repo in
    Format.asprintf "%s/-/graphs/%s" left branch
;;

let to_qexp obj =
  let open Paperwork.Qexp in
  match obj with
  | Github { username; name } as repo ->
    let kind = t_to_string repo in
    node [ keyword kind; atom username; atom name ]
  | Gitlab ({ username; name }, branch) as repo ->
    let kind = t_to_string repo in
    node [ keyword kind; atom username; atom name; string branch ]
;;

let sid x = Ok x

let from_qexp expr =
  let open Paperwork.Qexp in
  let open Validation.Infix in
  match expr with
  | Node [ kind; user; name ] ->
    (fun f user name -> f user name)
    <$> Mapper.token
          (fun s ->
            match ftrim s with
            | "github" -> Ok github
            | "gitlab" -> Ok (gitlab "master")
            | _ -> Error [ Of ("Invalid repo kind " ^ s) ])
          kind
    <*> Mapper.token sid user
    <*> Mapper.token sid name
  | Node [ kind; user; name; branch ] ->
    (fun f user name branch -> f branch user name)
    <$> Mapper.token
          (fun s ->
            match ftrim s with
            | "gitlab" -> Ok gitlab
            | _ -> Error [ Of ("Invalid repo kind " ^ s) ])
          kind
    <*> Mapper.token sid user
    <*> Mapper.token sid name
    <*> Mapper.token sid branch
  | _ -> Error [ Unparsable (to_string expr) ]
;;
