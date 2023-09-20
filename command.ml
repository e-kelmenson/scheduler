type object_phrase = string list

type feature_command =
  | Tasks of string
  | Events
  | Meeting
  | FHelp
  | Quit

exception Empty

exception Malformed

let parse_feature str = 
  let split = String.split_on_char ' ' str in   
  match List.filter (fun x -> not(x = "")) split with
  | [] -> raise Empty
  | h::[] ->
    if h = "quit" then Quit
    else if h = "events" then Events
    else if h = "meeting" then Meeting
    else raise Malformed
  | h::t::[] -> 
    if (h = "tasks" && t <> "") then Tasks t
    else raise Malformed
  | _ -> raise Malformed

