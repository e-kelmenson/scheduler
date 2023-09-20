open Tasks

type t = {
  tasks : task list;
  task_ids : task_id list;
  algo : string;
  map : (string * string) list
}

type result = Legal of t | Illegal

let init_state tsk_t = {
  tasks = get_tasks tsk_t;
  task_ids = task_ids tsk_t;
  algo = "SJF";
  map = []
}

let get_tasks st = st.tasks

let get_ids st = st.task_ids

let get_algo st = st.algo

let get_map st = st.map

(** [priority_compare t_lst a b] is a compare function for tasks that determines 
    if task [a] or task [b] is scheduled earlier based on their priority 
    found in [t_lst]. Because larger priority indicates higher importance, switch
    order in comparison of task [a] and [b] so higher priority task is 
    sorted earlier. *)
let priority_compare t_lst (a: task_id) (b: task_id) =
  Int.compare (priority t_lst b) (priority t_lst a)

(** [sjf_compare_helper t_lst a b] is a compare function for tasks that determines 
    if task [a] or task [b] is scheduled earlier based on their duration 
    found in [t_lst]. If durations are the same, schedule based on priority. *)
let sjf_compare_helper t_lst (a: task_id) (b: task_id) =
  let dl_a = duration t_lst a in
  let dl_b = duration t_lst b in
  if dl_a > dl_b then 1
  else if dl_a < dl_b then -1
  else priority_compare t_lst a b

(** [sjf_helper tasks t_lst] is the list of task_ids ordered according to the
    Shortest Job First algorithm. The duration of [tasks] is retrieved from
    [t_lst]. *)
let sjf_helper tasks t_lst = tasks |> List.sort (sjf_compare_helper t_lst)

(** [rr_queue_helper queue tasks t_lst] pushes task ids from [tasks] and their 
    duration which is retrieved from [t_lst] onto [queue].*)
let rr_queue_helper queue tasks t_lst =
  List.iter (fun x -> Queue.push (x, duration t_lst x) queue) tasks

(** [rr_rec_helper queue acc] is the list of tasks in [queue] according to
    round robin algorithm with quantum 1 hr.*)
let rec rr_rec_helper queue acc =
  if Queue.length queue = 0 then acc
  else 
    let t = Queue.pop queue in 
    match t with 
    | (id, dur) -> if dur > 1.0 then Queue.push (id, dur -. 1.0) queue else (); 
      let acc' = id::acc in rr_rec_helper queue acc'

(** [rr_helper tasks t_lst] is the list of task_ids ordered according to the
    Round Robin algorithm. The duration of [tasks] is retrieved from
    [t_lst]. The default quantum is defined in helper function [rr_rec_helper]. *)
let rr_helper tasks t_lst = 
  let queue = Queue.create () in 
  rr_queue_helper queue tasks t_lst; List.rev (rr_rec_helper queue [])

(** [edf_compare_helper t_lst a b] is a compare function for tasks that determines 
    if task [a] or task [b] is scheduled earlier based on their deadline 
    found in [t_lst]. If deadlines are the same, schedule based on priority. *)
let edf_compare_helper t_lst (a: task_id) (b: task_id) =
  let cmp = dl_compare t_lst a b in
  if cmp <> 0 then cmp else priority_compare t_lst a b

(** [edf_helper tasks t_lst] is the list of task_ids ordered according to the
    Earliest Job First algorithm. The deadline of [tasks] is retrieved from
    [t_lst]. *)
let edf_helper tasks t_lst = tasks |> List.sort (edf_compare_helper t_lst)

let schedule algo tsk_t st =
  if not(List.mem algo ["FCFS";"SJF";"RR";"EDF";"SJFRR";"EDFRR"]) then Illegal else
    let tasks' = List.map (fun x -> get_id x) st.tasks in
    let task_ids = 
      if algo = "FCFS" then task_ids tsk_t
      else if algo = "SJF" then sjf_helper tasks' tsk_t
      else if algo = "RR" then rr_helper tasks' tsk_t
      else if algo = "EDF" then edf_helper tasks' tsk_t 
      else if algo = "SJFRR" then rr_helper (sjf_helper tasks' tsk_t) tsk_t
      else if algo = "EDFRR" then rr_helper (edf_helper tasks' tsk_t) tsk_t
      else [] in
    Legal {
      tasks = st.tasks; 
      task_ids = task_ids; 
      algo = algo; 
      map = st.map
    }

let remove task_id tsk_t st = 
  let tsk = List.filter (fun x -> get_id x <> task_id) st.tasks in 
  let tsk_ids = task_ids tsk_t in 
  if tsk_ids = (task_ids tsk_t) then 
    Illegal 
  else 
    Legal {
      tasks = tsk; 
      task_ids = tsk_ids; 
      algo = st.algo; 
      map = st.map
    }

(** [extracted_task_id st_lst] is the task id of a list of parsed user input.
    Ex. if [st_lst] is [["2", "7.5", "07/08/2020", "cake", "a", "Baking"]] then
    the extracted task id is "Baking a cake". 
    Precondition: [st_lst] must be a reversed list of parsed user input. *)
let extracted_task_id st_lst =
  let st_lst' = st_lst |> List.tl |> List.tl |> List.tl in
  List.fold_left (fun x acc -> acc^" "^x) (List.hd st_lst') (st_lst' |> List.tl)

let add task st = 
  try begin
    let st_lst = String.split_on_char ' ' task |> List.rev in 
    let task_name = extracted_task_id st_lst in
    if List.mem task_name st.task_ids then 
      Illegal 
    else 
      let new_t = create_task (task_name) 
          (List.nth st_lst 2) (List.nth st_lst 1 |> float_of_string) 
          (st_lst |> List.hd |> int_of_string) in 
      Legal {
        tasks = new_t::st.tasks;
        task_ids = task_name::st.task_ids;
        algo = st.algo; 
        map = st.map
      }
  end 
  with _ -> Illegal

(** [firstn n lst acc] is a list of the first [n] number of task ids in [lst]. *)
let rec firstn n lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> 
    if (n = 0) then acc
    else firstn (n-1) t (h::acc)

let display n tsk_t st = 
  match schedule st.algo tsk_t st with
  | Legal t -> Legal {
      tasks = t.tasks; 
      task_ids = firstn n t.task_ids [] |> List.rev; 
      algo = t.algo;
      map = t.map
    }
  | Illegal -> Illegal

let modify task st = 
  try begin
    let st_lst = String.split_on_char ' ' task |> List.rev in
    let task_name = extracted_task_id st_lst in
    let new_t = create_task task_name (List.nth st_lst 2) 
        (List.nth st_lst 1 |> float_of_string) (st_lst |> List.hd |> int_of_string) in 
    if List.mem task_name st.task_ids then 
      let tsks = List.map 
          (fun x -> if get_id x = get_id new_t then new_t else x) st.tasks in 
      Legal {
        tasks = tsks; 
        task_ids = st.task_ids; 
        algo = st.algo; 
        map = st.map
      } 
    else 
      Illegal
  end 
  with _ -> Illegal

let print_map_helper st = 
  List.fold_left (fun acc x ->
      match x with
      | (k, v) -> acc@[k^": "^v]) [] st.map

(** [delegate_helper acc tsk_t st] returns name of person to be assigned to task. 
    Assignment is based on rounds where there is a set order of people delegation. *)
let delegate_helper acc tsk_t st =
  let lst = get_people tsk_t in
  let remainder = List.length acc mod (List.length lst) in
  get_name (List.nth lst remainder)

let delegate tsk_t st = 
  try begin
    let mapping = List.fold_left 
        (fun acc x -> acc@[(x, delegate_helper acc tsk_t st)])
        [] st.task_ids in
    Legal {
      tasks = st.tasks; 
      task_ids = st.task_ids; 
      algo = st.algo; 
      map = mapping
    } 
  end
  with _ -> Illegal