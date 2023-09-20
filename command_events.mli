type list_name = string
type event_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a string, string * string pair, string * string
    list pair, or string list depending on the command. *)
type command = 
  | NewList of list_name
  | AddFromFile of list_name * string
  | AddEvent of list_name * event_phrase
  | RemoveEvent of list_name * string
  | FindOpt of list_name
  | Display of list_name
  | ReturnAvailability of string list
  | Exit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command]. *)
val parse : string -> command

(** [parse_event phrase] parses an event phrase into an event. *)
val parse_event : event_phrase -> Events.event
