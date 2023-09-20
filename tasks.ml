open Yojson.Basic.Util

type task_id = string

exception UnknownTask of task_id
exception InvalidDate 

type date = {
  month: int;
  day: int;
  year: int
}

type task = {
  task_id: task_id; 
  deadline: date;
  duration: float;
  priority: int;
}

type person = {
  name: string;
  workload: int;
}

type t = {
  task_list : task list;
  people: person list;
}

(** [deadline_of_json dl] is the deadline that json [dl] represents. *)
let deadline_of_json dl = {
  month = dl |> member "month" |> to_string |> int_of_string;
  day = dl |> member "day" |> to_string |> int_of_string;
  year = dl |> member "year" |> to_string |> int_of_string;
}

(** [task_of_json tsk] is the task that json [tsk] represents. *)
let task_of_json tsk = {
  task_id = tsk |> member "id" |> to_string;
  deadline = tsk |> member "deadline" |> deadline_of_json;
  duration = tsk |> member "duration" |> to_string |> float_of_string;
  priority = tsk |> member "priority" |> to_string |> int_of_string;
}

(** [person_of_json] is the person that json [person] represents. *)
let person_of_json person = {
  name = person |> member "name" |> to_string;
  workload = person |> member "workload" |> to_string |> int_of_string;
}

let from_json json = {
  task_list = json |> member "tasks" |> to_list |> List.map task_of_json;
  people = json |> member "people" |> to_list |> List.map person_of_json;
}

let get_tasks t = t.task_list

let get_people t = t.people

let get_id task = task.task_id

let get_name person = person.name

let get_workload person = person.workload

let task_ids t = List.map (fun x -> x.task_id) t.task_list

let task_t t task_id = List.filter (fun x -> x.task_id = task_id) t.task_list

let duration t task_id =
  match task_t t task_id with
  | [] -> raise (UnknownTask task_id)
  | h::_ -> h.duration

let deadline t task_id =
  match task_t t task_id with
  | [] -> raise (UnknownTask task_id)
  | h::_ -> h.deadline

let priority t task_id =
  match task_t t task_id with
  | [] -> raise (UnknownTask task_id)
  | h::_ -> h.priority

(** [extract_deadline dl] is the date represenation of compatible string [dl].
    A compatible string is a string of the format "mm/dd/yyyy" (ex. "08/06/1998")
    Raises [InvalidDate] if [dl] is not a compatible string. *)
let extract_deadline dl = 
  let lst = String.split_on_char '/' dl in 
  if List.length lst <> 3 then
    raise InvalidDate
  else 
    { 
      month = List.nth lst 0 |> int_of_string; 
      day = List.nth lst 1 |> int_of_string; 
      year = List.nth lst 2 |> int_of_string
    }

let create_task id dl dur pri =
  {
    task_id = id; 
    deadline = extract_deadline dl; 
    duration = dur;
    priority = pri;
  }

let dl_compare t (a: task_id) (b: task_id) =
  let dl_a = deadline t a in
  let dl_b = deadline t b in
  if not(dl_a.year = dl_b.year) then Int.compare dl_a.year dl_b.year
  else if not(dl_a.month = dl_b.month) then Int.compare dl_a.month dl_b.month
  else if not(dl_a.day = dl_b.day) then Int.compare dl_a.day dl_b.day
  else 0