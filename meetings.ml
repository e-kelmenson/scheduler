open Yojson.Basic.Util

(* The type of meetings objects. Contains input to the meeting time problem. *)
type t = {
  people : person list;
  timeline : interval;
}
and person = {
  name : string;
  busy_times : interval list;
}
and interval = {
  start_time : int;
  end_time : int;
}

(* [interval_of_json json] is the interval object representing the json 
   [json]. *)
let interval_of_json json = {
  start_time = json |> member "start time" |> to_int;
  end_time = json |> member "end time" |> to_int;
}

(* [person_of_json json] is the person object representing the json [json]. *)
let person_of_json json = {
  name = json |> member "name" |> to_string;
  busy_times = json |> member "busy" |> to_list |> List.map interval_of_json;
}

let group_of_json json = {
  people = json |> member "people" |> to_list |> List.map person_of_json 
           |> List.rev;
  timeline = json |> member "timeline" |> interval_of_json;
}

(** [get_people g] is the list of people in group [g].  *)
let get_people g = g.people

(** [get_start i] is the start time of interval [i].  *)
let get_start i = i.start_time

(** [get_end i] is the end time of interval [i].  *)
let get_end i = i.end_time

(** [get_busy p] is list of intervals person [p] is busy during.  *)
let get_busy p = p.busy_times

(** [in_curlies s] is the string [{s}].  *)
let in_curlies s = "{"^s^"}"

(** [in_brackets s] is the string [[s]].  *)
let in_brackets s = "["^s^"]"

let create_interval s_t e_t = {
  start_time = s_t;
  end_time = e_t;
}

(** [periods start fin acc] is the list of intervals of duration 1 in the
    timeline starting at time [start] and ending at time [fin], where [acc]
    is an accumulator for the list. *)
let rec periods start fin acc =
  if start >= fin then acc
  else periods (start+1) fin ((create_interval start (start+1)) :: acc)

(** [potential_periods i] is the list of intervals of duration 1 in the 
    interval [i].  *)
let potential_periods i = 
  periods (get_start i) (get_end i) [] |> List.rev

(** [timeline_periods m] is the list of potential meeting periods for the group 
    (and its timeline) [m]. *)
let timeline_periods m = 
  potential_periods m.timeline

(** [update_conflicts periods person] is the list of potential periods
    [periods], with those that conflict with the person [person] removed.  *)
let update_conflicts periods person = 
  let conf = 
    person 
    |> get_busy 
    |> List.map potential_periods 
    |> List.flatten 
  in
  List.filter (fun x -> not (List.mem x conf)) periods

let empty = {
  people = [];
  timeline = create_interval 0 24;
}

let empty_intervals = []

let person_name p = p.name

let create_person id busy_periods = {
  name = id;
  busy_times = busy_periods;
}

let add_person p t = {
  t with people = p :: (get_people t);
}

let remove_person p_id t = {
  t with people = List.filter (fun x -> person_name x <> p_id) (get_people t)
}

let select_meetings group = 
  let init_periods = timeline_periods group in
  List.fold_left update_conflicts init_periods group.people

let to_string t = 
  t 
  |> get_people 
  |> List.map person_name 
  |> List.rev 
  |> String.concat ", " 
  |> in_brackets

let string_of_interval i = 
  "[" ^ string_of_int (get_start i) ^ ", " ^ string_of_int (get_end i) ^ "]"



