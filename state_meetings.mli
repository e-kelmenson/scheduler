open Meetings

(** 
   Representation of dynamic Groups state.

   This module represents the state of all Groups as the user interacts with
   them in Meeting Mode. A Group has a name, a group of people with varying
   availabilities, and a set of possible meeting times.
*)

(** The abstract type representing Meeting mode state.  *)
type t 

(** The abstract type representing a set of meeting times.  *)
type meetings


(** [init_state] is the initial state of Meeting mode.  *)
val init_state : t

(** [empty_meetings] is an empty set of meeting times.  *)
val empty_meetings : meetings

(** [get_group id st] is group named [id] in state [st].  *)
val get_group : string -> t -> Meetings.t

(** [get_meetings id st] is the set of meeting times for the group named [id] in 
    state [st].  *)
val get_meetings : string -> t -> meetings

(** [add_group id group st] is the new state [st'] resulting from adding a new
    group [group] named [id] in [st].  *)
val add_group : string -> Meetings.t -> t -> t

(** [add_from_json id file st] is the new state [st'] resulting from adding a 
    new group from [file] named [id] in [st].  *)
val add_from_json : string -> string -> t -> t

(** [add_person id person st] is the new state [st'] resulting from adding the
    person [person] to the group named [id] in [st].  *)
val add_person : string -> Meetings.person -> t -> t

(** [remove_person id person st] is the new state [st'] resulting from removing
    the person [person] from the group named [id] in [st].  *)
val remove_person : string -> string -> t -> t

(** [select_meetings id st] is the new state [st'] resulting from running the
    algorithm to find the optimal selection of meetings for the group named [id]
    in [st]. *)
val select_meetings : string -> t -> t 

(** [string_of_meetings id st] is the string representation of the possible 
    meeting times for the group named [id] in [st]. *)
val string_of_meetings : string -> t -> string

(** [to_string st] is the string representation of state [st]. *)
val to_string : t -> string
