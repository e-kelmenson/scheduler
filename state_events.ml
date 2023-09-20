open Events
open Stringhelp

type t = (string * Events.t) list
type event = Events.event


let init_state = []

let add_list name lst st = (name, lst) :: st

let add_from_json name file st = 
  let json = Yojson.Basic.from_file file in
  let events_list = events_of_json json in
  add_list name events_list st

let del_list name st = List.remove_assoc name st

(** [update_list id events st] is the new state [st'] resulting from updating 
    the events list named [id] to contain the events [events] in [st].  *)
let update_list name lst st = 
  st |> del_list name |> add_list name lst 

let get_events name st = List.assoc name st

let add_event name e st = 
  let lst' = get_events name st |> Events.add_event e in
  update_list name lst' st

let remove_event name e_id st = 
  let lst' = get_events name st |> Events.remove_event e_id in
  update_list name lst' st

let find_opt_events name st = 
  let opt_list = get_events name st |> select_events in
  update_list name opt_list st

let to_string st = 
  List.map (fun (x, y) -> x ^ ": " ^ Events.to_string y) st
  |> List.rev
  |> String.concat "; "
  |> in_brackets
