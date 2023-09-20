type list_name = string
type event_phrase = string list

type command = 
  | NewList of list_name 
  | AddFromFile of list_name * string
  | AddEvent of list_name * event_phrase
  | RemoveEvent of list_name * string
  | FindOpt of list_name
  | Display of list_name
  | ReturnAvailability of string list
  | Exit

exception Empty

exception Malformed

let parse str =
  let split = String.split_on_char ' ' str in 
  match List.filter (fun x -> x<>"") split with
  | [] -> raise Empty
  | h::[] -> 
    if h = "exit" then Exit 
    else raise Malformed
  | h::t::[] -> 
    if h = "newlist" then NewList t
    else if h = "findopt" then FindOpt t
    else if h = "display" then Display t
    else raise Malformed
  | h :: h' :: t :: [] ->
    if h = "addfromfile" then AddFromFile (h', t) 
    else if h = "removeevent" then RemoveEvent (h', t) 
    else raise Malformed
  | h :: h' :: t ->
    if h = "addevent" then AddEvent (h', t)
    else if h = "returnavailability" && (List.length t >= 9) 
    then ReturnAvailability (h'::t)
    else raise Malformed

let parse_event phrase = match phrase with
  | n :: s_t :: e_t :: w :: [] -> 
    Events.create_event n (int_of_string s_t) (int_of_string e_t) (int_of_string w)
  | _ -> raise Malformed