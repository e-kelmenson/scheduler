open Yojson.Basic.Util
open Stringhelp

(* The goal of the weighted interval scheduling algorithm is to find a 
   maximal set of compatible jobs. We will let jobs be events with fixed start
   and end times, such as review sessions, which each have a weight. This weight 
   will be their expected benefit to the user. This will be based on how they will
   increase the overall grade. For now, weights will be assigned specifically but
   this can be extended so that each job has an expected benefit for a particular
   class, and we can maximize the total expected benefit to GPA. *)

type t = event list

and event = {
  name : string;
  start_time : int;
  end_time : int;
  weight : int
}

(** [event_of_json j] is the event that [j] represents.
    Requires: [j] is a valid JSON event object representation.  *)
let event_of_json j = {
  name = j |> member "id" |> to_string;
  start_time = j |> member "start time" |> to_string |> int_of_string;
  end_time = j |> member "end time" |> to_string |> int_of_string;
  weight = j |> member "weight" |> to_string |> int_of_string;
}

let get_name e = e.name

let get_start e = e.start_time

let get_end e = e.end_time

let get_weight e = e.weight

let empty = []

let events_of_json json = 
  json |> member "events" |> to_list |> List.map (event_of_json) |> List.rev

(* [compare_events e1 e2] is [-1] if event [e1] ends before event [e2], [0] if 
   they end at at the same time, and [1] if [e1] ends after [e2]. *)
let compare_events e1 e2 = Stdlib.compare (get_end e1) (get_end e2)

(* [sorted_events elist] is a list of the events in [elist], sorted in 
   non-decreasing order by finish time. *)
let sorted_events elist = List.sort compare_events elist

(* [numbered_events elist] is an association list of the events in [elist] where
   the keys are indices and values are events. *)
let numbered_events elist = List.mapi (fun i e -> (i, e)) elist

(* [max_compat i lst] is [Some e] where [e] is the latest event which finishes 
   strictly before time [t] in the list of jobs [lst]. If there is no such 
   event, then it is [None]. *)
let rec max_compat t lst = 
  List.fold_left (fun acc (i,j) -> if j.end_time < t then (Some i) else acc) 
    None lst

(** [get_max arr i_opt] is the opt value at index [i_opt] in the array [arr]. 
    If [i_opt] is [None], it is [0].*)
let get_max arr i_opt = match i_opt with
  | None -> 0
  | Some i -> arr.(i)

(** [opt_arr_init n] is an array of length [n] initialized with a value of 0
    at each index. *)
let opt_arr_init n = Array.make n 0

(** [opt_helper arr elist i] computes the opt value at index [i] in [arr] according
    to the following algorithm:
    if [i] = 0 then w_i, the weight of event at index [i] in [elist]
    else max {arr[i-1], w_i + max j s.t f[j] < s[i] {arr[j]} } *)
let opt_helper arr elist i = 
  let w_i = List.assoc i elist |> get_weight in
  if i = 0 then w_i
  else 
    let t = List.assoc i elist |> get_start in
    max (arr.(i-1)) (max_compat t elist |> get_max arr |> (+) w_i)

(** [opt_arr arr lst] iterates through the opt array [arr] and computes
    the opt values.  *)
let opt_arr arr lst = 
  Array.iteri (fun i elem -> arr.(i) <- opt_helper arr lst i) arr

(** [find_sol arr lst i acc] *)
let rec find_sol arr lst i acc = 
  if i = 0 then acc
  else 
    let w_i = List.assoc i lst |> get_weight in
    let t = List.assoc i lst |> get_start in
    if (arr.(i-1)) >= (max_compat t lst |> get_max arr |> (+) w_i) 
    then find_sol arr lst (i-1) acc
    else match max_compat t lst with
      | Some j -> find_sol arr lst j (i::acc)
      | None -> i::acc

(** [events_from_indices ilst elist] is the list of events corresponding to the 
    indices in the list [ilst], in the indexed event list [elist].*)
let events_from_indices ilst elist = 
  List.map (fun i -> List.assoc i elist) ilst

let add_event e t =
  e :: t

let create_event name s_t e_t w = {
  name = name;
  start_time = s_t;
  end_time = e_t;
  weight = w;
}

let remove_event e_id t =
  List.filter (fun x -> get_name x <> e_id) t

let select_events t =
  let index_events = t |> sorted_events |> numbered_events in
  let n = List.length index_events in
  let opt = opt_arr_init n in
  let _ = opt_arr opt index_events in
  events_from_indices (find_sol opt index_events (n-1) []) index_events

let string_of_event e = 
  "id: " 
  ^ (get_name e) 
  ^ ", period: [" 
  ^ string_of_int (get_start e) 
  ^ ", " 
  ^ string_of_int (get_end e) 
  ^ "], weight: " 
  ^ string_of_int (get_weight e) 
  |> in_curlies

let to_string j_list = 
  j_list 
  |> List.rev
  |> List.map string_of_event
  |> String.concat ", " 
  |> in_brackets

(** [available_members lst name stime sd sm sy etime ed em ey acc] is the number
    of people from lst, added to the current number of people available,
     who are available for a specified meeting time. *)
let rec available_members lst name stime sd sm sy etime ed em ey acc = 
  match lst with 
  | [] -> acc
  | h::t -> 
    if (Geticalinfo.add_to_calendar h name stime sd sm sy etime ed em ey) then 
      available_members t name stime sd sm sy etime ed em ey (acc+1) 
    else 
      available_members t name stime sd sm sy etime ed em ey (acc) 

let rec print_lst lst = 
  match lst with 
  | [] -> ()
  | h::t -> print_endline h;
    print_lst t

let availability files name stime sd sm sy etime ed em ey =
  if files = "" then 0
  else 
    let lst = String.split_on_char ' ' files in
    available_members lst name stime sd sm sy etime ed em ey 0