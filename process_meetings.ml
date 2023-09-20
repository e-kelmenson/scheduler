open Meetings
open State_meetings
open Command_meetings
(* LOC = 53 *)


let print_statement statement = 
  print_endline statement;
  print_string "Please enter a command \n> "

let rec process_command state =
  begin
    match Command_meetings.parse (read_line ()) with
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
      | NewGroup name -> begin
          let state' = add_group name Meetings.empty state in
          print_statement ("Created new group " ^ name);
          process_command state'
        end
      | FindMeetings name -> begin
          let state' = State_meetings.select_meetings name state in
          print_statement ("Computed optimal selection of events in " ^ name);
          process_command state'
        end
      | DisplayGroup name -> begin
          print_endline ("Group " ^ name ^ ": ");
          print_statement (State_meetings.get_group name state |> Meetings.to_string);
          process_command state
        end
      | DisplayMeetings name -> begin
          print_endline ("Group " ^ name ^ " has possible meeting times: ");
          print_statement (State_meetings.string_of_meetings name state);
          process_command state
        end
      | AddFromFile (name, file) -> begin
          let state' = State_meetings.add_from_json name file state in
          print_statement ("Added group from " ^ file ^ " to " ^ name);
          process_command state'
        end
      | AddPerson (name, p_id, person_phrase) -> begin
          let person = parse_person p_id person_phrase in
          let state' = add_person name person state in
          "Added person " ^ Meetings.person_name person ^ " to " ^ name
          |> print_statement;
          process_command state'
        end
      | RemovePerson (name, person) -> begin
          let state' = remove_person name person state in
          "Removed person " ^ person ^ " from " ^ name
          |> print_statement;
          process_command state'
        end
  end



