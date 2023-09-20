(** The abstract type of values representing events list objects. *)
type t

(** The abstract type of values representing events. *)
type event

(** [get_name e] is the name of event [e].  *)
val get_name : event -> string

(** [get_start e] is the start time of event [e]. *)
val get_start : event -> int

(** [get_end e] is the end time of event [e]. *)
val get_end : event -> int

(** [get_weight e] is the weight of event [e]. *)
val get_weight : event -> int

(** [empty] is an empty events list.  *)
val empty : t

(** [events_of_json j] is the events list that [j] represents.
    Requires: [j] is a valid JSON events list object representation. *)
val events_of_json : Yojson.Basic.t -> t

(** [create_event name s_t e_t w] is the event with name [name] which starts at
    [s_t], ends at [e_t], and has a weight [w].  *)
val create_event : string -> int -> int -> int -> event

(** [add_event e events] is the events list [events] with event [e] added.  *)
val add_event : event -> t -> t

(** [remove_event e events] is the events list [events] with event [e] 
    removed.  *)
val remove_event : string -> t -> t

(** [select_events events] is the events list object consisting of the optimal
    subset of non-conflicting events in [events].  *)
val select_events : t -> t

(** [to_string events] is the string representation of the events list 
    [events].  *)
val to_string : t -> string

(** [string_of_event event] is the string representation of the event 
    [event].  *)
val string_of_event : event -> string

(** [availibility lst name stime sd sm sy etime ed em ey] is the number of 
    people whose calendar files are in lst who will be available for meeting 
    [name], starting from [stime] on [sd] [sm] [sy] and ending on [etime] on 
    [ed] [em] [ey]. *)
val availability : string -> string -> int -> int -> int -> 
  int -> int -> int -> int -> int -> int 
