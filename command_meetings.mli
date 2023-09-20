type group_name = string
type person_phrase = string 

(** The type [command] represents a player command that is decomposed
    into a verb and possibly string, string * string pair, or string * string
    list pair. *)
type command = 
  | NewGroup of group_name
  | AddFromFile of group_name * string
  | AddPerson of group_name * string * person_phrase
  | RemovePerson of group_name * string
  | FindMeetings of group_name
  | DisplayGroup of group_name
  | DisplayMeetings of group_name
  | Exit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command]. *)
val parse : string -> command 

(** [parse_person phrase] parses a [person_phrase] into a [person]. *)
val parse_person : string -> person_phrase -> Meetings.person 