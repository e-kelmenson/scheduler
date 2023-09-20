(** The type of task identifiers. *)
type task_id = string

(** Exception for task_id having no corresponding task. *)
exception UnknownTask of task_id
(** Exception for date format being invalid. *)
exception InvalidDate 

(** The type of a deadline for a task. *)
type date

(** The type of a task in the schedule. *)
type task

(** The type of a person in t. *)
type person

(** The abstract type representing a group of tasks and people to be assigned to them. *)
type t

(** [from_json j] is the task group that [j] represents.
    Requires: [j] is a valid JSON task representation. *)
val from_json : Yojson.Basic.t -> t

(** [get_tasks t] is the task list of [t]. *)
val get_tasks : t -> task list

(** [get_people t] is the people list of [t]. *)
val get_people : t -> person list

(** [get_id task] is the task_id of [task]. *)
val get_id : task -> task_id

(** [get_name person] is the name of [person]. *)
val get_name : person -> string

(** [get_workload person] is the workload of [person]. *)
val get_workload : person -> int

(** [task_ids tasks] is a list of all of the task identifiers from tasks in [t].*)
val task_ids : t -> string list

(** [task_t t task_id] is a list of tasks in [t] with id equal to [task_id].
    Empty list if [task_id] matches no task. *)
val task_t : t -> task_id -> task list

(** [duration t task_id] is the duration of task with [task_id] in [t]. 
    Raises [UnknownTask task_id] if [task_id] is not a task identifier in [t]. *)
val duration : t -> task_id -> float

(** [deadline t task_id] is the deadline of task with [task_id] in [t]. 
    Raises [UnknownTask task_id] if [task_id] is not a task identifier in [t]. *)
val deadline : t -> task_id -> date

(** [priority t task_id] is the priority of task with [task_id] in [t]. 
    Raises [UnknownTask task_id] if [task_id] is not a task identifier in [t]. *)
val priority : t -> task_id -> int

(** [create_task id dl dur pri] is a new task [t] with task name [id], deadline
    [dl], duration [dur], and priority [pri].
    Requires [dl] to be properly formatted string of "mm/dd/yyyy" format.
    Raises [InvalidDate] if [dl] is not properly formatted string. *)
val create_task : task_id -> string -> float -> int -> task

(** [dl_compare t task_id task_id] is the compare function for deadlines which
    returns int based on which task with [task_id] has earlier deadline. *)
val dl_compare : t -> task_id -> task_id -> int