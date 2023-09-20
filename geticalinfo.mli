(** [event] represents an event with its name, date and time. *)
type event = {
  name : string;
  start_time : int;
  start_day: int;
  start_month: int;
  start_year: int;
  end_time : int;
  end_day : int;
  end_month : int;
  end_year : int;
}

(** [add_to_calendar file name stime sd sm sy etime ed em ey] is true if the 
    requested event can be added to the existing calendar.  *)
val add_to_calendar : string -> string -> int -> int -> int -> 
  int -> int -> int -> int -> int -> bool

