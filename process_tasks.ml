open Tasks
open State_tasks
open Command_tasks

(** [print_statement msg] prints message and a request for a new command. *)
let print_statement msg = 
  print_endline msg;
  print_string "Please enter a command \n> "

(** [t_help_msg] is the help message that contains information on tasks. *)
let t_help_msg = 
  "Task commands includ:
    exit: exits application
    help: shows command descriptions
    delegate: assigns people to tasks in file
    schedule <algo>: schedules according to algo where algo is one of SJF, EDF, RR FCFS, SJFRR, or EDFRR
    remove <task>: removes task from schedule
    add <name date duration priority>: adds task with specified properties to schedule
    display <number>: shows the first specified number of tasks to be completed
    modify <name date duration priority>: modifies task with new specified properties \n"

(** [delegate_state sch state] is [state] if delegation fails. Otherwise, it is 
    [state'] containing updated information after delegation. *)
let delegate_state sch state = begin
  let result = delegate sch state in
  match result with 
  | Illegal -> 
    print_statement "Error: Tasks cannot be delegated.";
    state;
  | Legal new_state -> 
    print_endline "Your task delegation is";
    new_state |> print_map_helper |> (String.concat " | ") |> print_statement;
    new_state;
end

(** [schedule_state algo sch state] is [state] if scheduling fails. Otherwise, 
    it is [state'] containing updated information after scheduling. *)
let schedule_state algo sch state = begin
  let result = schedule algo sch state in
  match result with 
  | Illegal -> 
    print_statement "Error: Schedule cannot be formed.";
    state;
  | Legal new_state -> 
    print_endline "Your schedule is \n";
    new_state |> get_ids |> (String.concat " | ") |> print_statement;
    new_state;
end

(** [remove_state task sch state] is [state] if removal of task fails. 
    Otherwise, it is [state'] containing updated information after removal. *)
let remove_state task sch state = begin
  let result = remove (String.concat " " task) sch state in
  match result with 
  | Illegal -> 
    print_statement "Error: Invalid task.";
    state;
  | Legal new_state -> 
    print_statement ("You removed "^(String.concat " " task)^" from your schedule");
    new_state;
end

(** [add_state task sch state] is [state] if creation of [task] fails. 
    Otherwise, it is [state'] containing updated information after creation. *)
let add_state task sch state = begin
  let result = add (String.concat " " task) state in 
  match result with
  | Illegal -> 
    print_statement "Error: Task already exists! Try the modify command instead.";
    state;
  | Legal new_state -> 
    print_statement ("You have successfully added " ^ (String.concat " " task) ^ " to your schedule.");
    new_state;
end

(** [schedule_state k sch state] is [state]. It will also print a list of first 
    [k] scheduled task names. *)
let display_state k sch state = begin
  let result = display k sch state in 
  match result with
  | Illegal -> 
    print_statement "Error: This should never happen.";
    state;
  | Legal new_state -> new_state |> get_ids |> String.concat " | " |> print_statement;
    state;
end

(** [modify_state task sch state] is [state] if modification of [task] fails. 
    Otherwise, it is [state'] containing updated information after modification. *)
let modify_state task state = begin
  let result = modify (String.concat " " task) state in 
  match result with
  |Illegal -> 
    print_statement "Error: This task does not currently exist. Try the add command instead.";
    state;
  |Legal new_state -> 
    print_statement ("You have successfully modified " ^ (String.concat " " task) ^ " ");
    new_state;
end

(** [process_command sch state] performs appropriate action depending on
    input of user. Exluding an attempt at quiting application, 
    [process_command] will continue calling itself with potentially a
    new state after each user input. *)
let rec process_task_command sch state = begin
  match parse_task (read_line ()) with
  | exception Empty ->  
    print_statement "Error: No command entered.";
    process_task_command sch state;
  | exception Malformed ->
    print_statement "Error: Malformed command."; 
    process_task_command sch state;
  | valid_command -> 
    match valid_command with 
    | Exit -> 
      print_endline "Exiting task scheduler."; exit 0;
    | THelp ->
      print_statement t_help_msg;
      process_task_command sch state;
    | command -> let state' = begin match command with
        | Delegate -> delegate_state sch state
        | Schedule algo -> schedule_state algo sch state
        | Remove task -> remove_state task sch state
        | Add task -> add_state task sch state
        | Display k -> display_state k sch state
        | Modify task -> modify_state task state
        | _ -> failwith "Error: Unknown command."
      end
      in process_task_command sch state'
end

let scheduler file =
  let json = Yojson.Basic.from_file file in 
  let sch = from_json json in
  let i_state = init_state sch in
  "Please edit schedule or print schedule \n" |> print_statement;
  process_task_command sch i_state