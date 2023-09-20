(** The type [feature_command] represents a player command that is decomposed
    into a verb and possibly a string. *)
type feature_command =
  | Tasks of string
  | Events
  | Meeting
  | FHelp
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse_feature str] parses a player's input into a [feature_command]. *)
val parse_feature : string -> feature_command