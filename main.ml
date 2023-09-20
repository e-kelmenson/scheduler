open Command

let print_statement statement = 
  print_endline statement;
  print_string "Please enter a command \n> "

let f_help_msg = 
  "Type one of tasks <file>, events <file>, or meeting <file> to start scheduling.
      Use tasks for individual items that need to be completed. Use events to track upcoming
      and recurrant events. Use meeting to schedule meetings. \n"

let rec use_feature _ =
  match read_line () with
  | exception End_of_file -> ()
  | str ->
    begin
      match parse_feature (str) with
      | exception Empty ->  
        print_statement "Error: No command entered";
        use_feature ()
      | exception Malformed ->
        print_statement "Error: Malformed command"; 
        use_feature ()
      | valid_command -> 
        match valid_command with 
        | Quit -> 
          print_endline "Exiting the program."; exit 0;
        | FHelp ->
          print_statement f_help_msg; 
          use_feature ()
        | Tasks t -> 
          print_statement "Entering Task mode.";
          Process_tasks.scheduler t
        | Events -> 
          print_statement "Entering Event mode.";
          Process_events.process_command State_events.init_state
        | Meeting ->
          print_statement "Entering Meeting mode.";
          Process_meetings.process_command State_meetings.init_state
    end

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Final Project: Scheduler.\n");
  print_endline ("Please enter a command to use a feature and a file if applicable. " ^
                 "Otherwise, enter <command> <empty_string>\n");
  print_string  "> ";
  use_feature ()

let () = main ()