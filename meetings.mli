(** The abstract type representing a group object.  *)
type t

(** The abstract type representing a time interval.  *)
type interval 

(** The abstract type representing a person.  *)
type person

(** [empty] is an empty group object.  *)
val empty : t

(** [empty_intervals] is an empty list of intervals.  *)
val empty_intervals : interval list

(** [person_name person] is the name of the person [person].  *)
val person_name : person -> string

(** [create_person id i_list] is a new person with name [id] and busy times
    [i_list].  *)
val create_person : string -> interval list -> person

(** [create_interval s_t e_t] is a new interval that starts at [s_t] and ends
    at [e_t].  *)
val create_interval : int -> int -> interval

(** [add_person person group] is the group [group] with the person [person]
    added. *)
val add_person : person -> t -> t

(** [remove_person name group] is the group [group] with the person named
    [name] removed. *)
val remove_person : string -> t -> t

(** [select_meetings group] is a list of meeting times for which every person
    in [group] is available during. *)
val select_meetings : t -> interval list

(** [group_of_json j] is the group object that [j] represents.
    Requires: [j] is a valid JSON group object representation. *)
val group_of_json : Yojson.Basic.t -> t

(** [string_of_interval i] is the string representation of time interval 
    [i].  *)
val string_of_interval : interval -> string

(** [to_string group] is the string representation of the group [group].  *)
val to_string : t -> string