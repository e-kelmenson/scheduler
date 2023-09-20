type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possible a string, int or string list. *)
type command = 
  | Schedule of string
  | Exit
  | THelp
  | Remove of object_phrase
  | Add of object_phrase
  | Display of int
  | Modify of object_phrase
  | Delegate

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse_task str] parses a player's input into a [command]. *)
val parse_task : string -> command