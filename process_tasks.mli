(** [scheduler file] turns [file] into a Task.t and State_task.t representation
    and begins processing user input with the initial schedule and state. 
    Requires file to be json file with valid task representation. *)
val scheduler : string -> unit