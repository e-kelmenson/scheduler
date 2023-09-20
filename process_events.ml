open Events
open State_events
open Command_events
(* LOC = 53 *)


let print_statement statement = 
  print_endline statement;
  print_string "Please enter a command \n> "

let rec process_command state =
  begin
    match Command_events.parse (read_line ()) with
    | exception Empty ->  
      print_statement "Error: No command entered";
      process_command state;
    | exception Malformed ->
      print_statement "Error: Malformed command"; 
      process_command state;
    | valid_command -> 
      match valid_command with 
      | Exit -> 
        print_endline "Exiting events mode."; exit 0;
      | NewList name -> begin
          let state' = State_events.add_list name Events.empty state in
          print_statement ("Created new list " ^ name);
          process_command state'
        end
      | FindOpt name -> begin
          let state' = State_events.find_opt_events name state in
          print_statement ("Computed optimal selection of events in " ^ name);
          process_command state'
        end
      | Display name -> begin
          print_endline ("Events List " ^ name ^ ": ");
          print_statement (State_events.get_events name state 
                           |> Events.to_string);
          process_command state
        end
      | AddFromFile (name, file) -> begin
          let state' = State_events.add_from_json name file state in
          print_statement ("Added events list from " ^ file ^ " to " ^ name);
          process_command state'
        end
      | AddEvent (name, event_phrase) -> begin
          let event = parse_event event_phrase in
          let state' = add_event name event state in
          "Added event " ^ Events.get_name event ^ " to " ^ name
          |> print_statement;
          process_command state'
        end
      | RemoveEvent (name, event) -> begin
          let state' = remove_event name event state in
          "Removed event " ^ event ^ " from " ^ name
          |> print_statement;
          process_command state'
        end
      | ReturnAvailability lst -> 
        let n_lst = List.rev lst in 
        let v = begin 
          match n_lst with
          | p0::p1::p2::p3::p4::p5::p6::p7::p8::[] -> 
            availability "" p8 (int_of_string p7) (int_of_string p6) 
              (int_of_string p5) (int_of_string p4) (int_of_string p3) 
              (int_of_string p2) (int_of_string p1) (int_of_string p0)
          | p0::p1::p2::p3::p4::p5::p6::p7::p8::t -> 
            availability (String.concat " " t) p8 (int_of_string p7) 
              (int_of_string p6) (int_of_string p5) (int_of_string p4) 
              (int_of_string p3) (int_of_string p2) (int_of_string p1) 
              (int_of_string p0)
          | _ -> 0
        end in "There are " ^ (string_of_int v) ^ " people available for the " ^
               "requested event" |> print_statement;
        process_command state
  end



