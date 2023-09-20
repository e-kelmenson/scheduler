type object_phrase = string list

type command = 
  | Schedule of string
  | Exit
  | THelp
  | Remove of object_phrase
  | Add of object_phrase 
  | Display of int
  | Modify of object_phrase
  | Delegate

exception Empty

exception Malformed

let parse_task str =
  let split = String.split_on_char ' ' str in 
  match List.filter (fun x -> x<>"") split with
  | [] -> raise Empty
  | h::[] -> 
    if h = "exit" then Exit
    else if h = "delegate" then Delegate
    else if h = "help" then THelp
    else raise Malformed
  | h::t::[] -> 
    if h = "schedule" then Schedule t
    else if h = "display" then
      begin 
        match int_of_string t with 
        | exception _ -> raise Malformed
        | k -> Display k
      end
    else raise Malformed
  | h::t ->
    if h = "remove" then Remove t 
    else if h = "add" then Add t
    else if h = "modify" then Modify t 
    else raise Malformed