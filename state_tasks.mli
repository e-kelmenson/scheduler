(** The abstract type representing a task schedule. *)
type t

(** The type representing the result of executing a command. *)
type result = Legal of t | Illegal

(** [init_state tsk_t] is the initial state of a schedule given the information 
    [tsk_t] from json. The initial state defaults to a Shortest Job First algorithm. *)
val init_state : Tasks.t -> t

(** [get_tasks t] is the task list of state [t]. *)
val get_tasks : t -> Tasks.task list

(** [get_ids t] is the task_id list of state [t]. *)
val get_ids : t -> Tasks.task_id list

(** [get_algo t] is the algorithm of state [t]. *)
val get_algo : t -> string

(** [get_map t] is the task to people mapping of state [t]. *)
val get_map : t -> (string * string) list

(** [schedule a tsk_t t] is [r] if attempting to schedule at state [t] with
    scheduling algorithm [a]. If [a] is a valid algorithm, then [r] is [Legal t'].
    If [a] is invalid algorithm, then [r] is [Illegal]. *)
val schedule : string -> Tasks.t -> t -> result

(** [remove id tsk_t t] is [r] if attempting to remove task with [id] from state 
    [t]. If [id] is a valid id, then [r] is [Legal t']. If [id] is invalid id, 
    then [r] is [Illegal]. *)
val remove : string -> Tasks.t -> t -> result

(** [add task t] is [r] if attempting to add [task] to schedule. If successfully 
    added task t to schedule, then [r] is [Legal t']. If [task] has same id 
    as a current task in the schedule or if [task] is malformed, [r] is [Illegal]. *)
val add : string -> t -> result

(** [display n tsk_t t] is [r] if attempting to display first [n] tasks in scheduled
    order. If displayed succesffully, then [r] is [Legal t']. If unschedulable,
    then [r] is [Illegal]. 
    Precondition: [n] must be >=0. *)
val display : int -> Tasks.t -> t -> result

(** [modify task t] is [r] if attempting to modify [task] in schedule. If successfully 
    edited task t to schedule, then [r] is [Legal t']. If [task] has an id that does
    not match any of the current tasks in the schedule or if [task] is malformed,
    [r] is [Illegal]. *)
val modify : string -> t -> result

(** [print_map_helper t] is list of strings of mapping in [t] in order to make
    printing the map easier.
    Example: if mapping in [t] is [[("YT", "Susan"); ("G", "Sundar")]], then 
    print_map_helper returns [["YT: Susan"; "G: Sundar"]] *)
val print_map_helper : t -> string list

(** [delegate tsk_t t] is [r] if attempting to assign tasks to people in schedule.
    If successfully assigned tasks, then [r] is [Legal t']. If any exception is
    raised, [r] is [Illegal].*)
val delegate : Tasks.t -> t -> result