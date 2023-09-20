open Meetings
open Stringhelp

type t = (string * (Meetings.t * meetings)) list
and meetings = Meetings.interval list


let init_state = []

let empty_meetings = []

let add_group name group st = (name, (group, empty_meetings)) :: st

let add_t name group meetings st = (name, (group, meetings)) :: st

let del_t name st = List.remove_assoc name st

(** [get_group name st] is the group of people for the group named [name] in 
    [st]. *)
let get_group name st = List.assoc name st |> fst

(** [get_meetings name st] is the list of possible meeting times for the group
    named [name] in [st]. *)
let get_meetings name st = List.assoc name st |> snd

(** [update_group name meetings st] is the new state [st'] resulting from 
    updating the group named [name] to have the group of people [group] in 
    [st].  *)
let update_group name group st = 
  let meetings = get_meetings name st in
  del_t name st |> add_t name group meetings

(** [update_meetings name meetings st] is the new state [st'] resulting from 
    updating the group named [name] to have possible meeting times [meetings] in 
    [st].  *)
let update_meetings name meetings st =
  let group = get_group name st in
  del_t name st |> add_t name group meetings

let add_from_json name file st = 
  let json = Yojson.Basic.from_file file in
  let group = group_of_json json in
  add_group name group st

let add_person name p st = 
  let group' = get_group name st |> Meetings.add_person p in
  update_group name group' st

let remove_person name p_id st = 
  let group' = get_group name st |> Meetings.remove_person p_id in
  update_group name group' st

let select_meetings name st = 
  let meetings_list = get_group name st |> select_meetings in
  update_meetings name meetings_list st

let string_of_meetings name st = 
  let m = get_meetings name st in
  List.map Meetings.string_of_interval m |> String.concat ", " 
  |> in_brackets

let to_string st = 
  List.map (fun (x, (g, m)) -> 
      "Group " ^ x ^ ": " 
      ^ (Meetings.to_string g) 
      ^ " -> Meeting Times: " 
      ^ string_of_meetings x st) st
  |> List.rev
  |> String.concat "; "
  |> in_brackets