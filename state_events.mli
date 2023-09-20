open Events

(** 
   Representation of dynamic Events state.

   This module represents the state of all Events Lists as the user interacts 
   with them in Events Mode. An Events List has a name and a set of events.
*)

(** The abstract type representing the state in Events mode. *)
type t 

(** [init_state] is the initial state of Events mode.  *)
val init_state : t

(** [get_events id st] is the set of events in the events list named [id] in 
    state [st].  *)
val get_events : string -> t -> Events.t

(** [add_list id events st] is the new state [st'] resulting from adding a new
    events list [events] named [id] in [st].  *)
val add_list : string -> Events.t -> t -> t

(** [add_from_json id file st] is the new state [st'] resulting from adding a 
    new events list from [file] named [id] in [st].  *)
val add_from_json : string -> string -> t -> t

(** [del_list id st] is the new state [st'] resulting from deleting the events
    list named [id] in [st].  *)
val del_list : string -> t -> t

(** [add_event id event st] is the new state [st'] resulting from adding the
    event [event] to the events list named [id] in [st].  *)
val add_event : string -> Events.event -> t -> t

(** [remove_event id event_id st] is the new state [st'] resulting from removing 
    the event named [event_id] from the events list named [id] in [st].  *)
val remove_event : string -> string -> t -> t

(** [find_opt_events id st] is the new state [st'] resulting from running the
    algorithm to find the optimal selection of non-conflicting events from the 
    events list named [id] in [st]. *)
val find_opt_events : string -> t -> t

(** [to_string st] is the string representation of the state [st].  *)
val to_string : t -> string
