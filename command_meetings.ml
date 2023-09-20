
type group_name = string
type person_phrase = string

type command = 
  | NewGroup of group_name
  | AddFromFile of group_name * string
  | AddPerson of group_name * string * person_phrase
  | RemovePerson of group_name * string
  | FindMeetings of group_name
  | DisplayGroup of group_name
  | DisplayMeetings of group_name
  | Exit

exception Empty

exception Malformed

let str_to_lst str split = String.split_on_char split str |> 
                           List.filter (fun x -> not(x = ""))

let parse str =
  match str_to_lst str ' ' with
  | [] -> raise Empty
  | h::[] -> 
    if h = "exit" then Exit 
    else raise Malformed
  | h::t::[] -> 
    if h = "newgroup" then NewGroup t
    else if h = "findmeetings" then FindMeetings t
    else if h = "displaygroup" then DisplayGroup t
    else if h = "displaymeetings" then DisplayMeetings t
    else raise Malformed
  | h :: h' :: t :: [] ->
    if h = "addfromfile" then AddFromFile (h', t) 
    else if h = "removeperson" then RemovePerson (h', t) 
    else raise Malformed
  | h :: h' :: t :: t' :: [] ->
    if h = "addperson" then AddPerson (h', t, t')
    else raise Malformed
  | _ -> raise Malformed

let parse_interval str = match str_to_lst str ',' with
  | h :: t :: [] -> Meetings.create_interval (int_of_string h) (int_of_string t)
  | _ -> raise Malformed

let parse_interval_list str = match str_to_lst str ';' with
  | [] -> Meetings.empty_intervals
  | lst -> List.map (fun x -> parse_interval x) lst

let parse_person p_id phrase = 
  Meetings.create_person p_id (parse_interval_list phrase)



