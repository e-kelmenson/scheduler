open Str

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

(** [create_calendar_event n stime sd sm sy etime ed em eyear] is an [event] with  
    name = [n], start_time = [stime], start_day = [sd], start_year = [sy], 
    end_time = [etime], end_day = [ed], end_month = [em] and end_year = [ed]. *)
let create_calendar_event name stime sd sm sy etime ed em ey = 
  {name = name; start_time = stime; start_day = sd; start_month = sm;
   start_year = sy; end_time = etime; end_day = ed; end_month = em; 
   end_year = ey}

(** [events] is a reference to all events in the calendar. *)
let events = ref []

(** [contains s2 s1] is true if [s2] is a substring of [s1]. False otherwise. *) 
let contains s2 s1 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

(** [find_str st reg exp] returns a substring of [st] based on the 
    location of the first instance of the regex [reg] in [st] and [exp]. 
    Requires: [exp] is either Str.before or Str.after. *)
let find_str st reg exp = 
  let b_1 = Str.string_match (Str.regexp reg) st 0 in 
  if b_1 then 
    let loc = Str.match_beginning() in 
    if (exp = Str.string_after) then 
      exp st (loc+1) 
    else exp st loc
  else ""

(** [extract_event st ed name] is an event [name] with starting information [st] 
    and ending information [ed]. 
    Requires: [st] and [ed] must be DTSTART or DTEND instances from an 
    icalendar. *) 
let extract_event st ed name = 
  let st_str = String.split_on_char ':' st |> List.tl |> List.hd |> String.trim in 
  let end_str = String.split_on_char ':' ed |> List.tl |> List.hd |> String.trim in 
  if (st_str <> "" && end_str <> "") then 
    let st_time = String.split_on_char 'T' st_str |> List.tl |> List.hd 
                  |> int_of_string in 
    let ed_time = String.split_on_char 'T' end_str |> List.tl |> List.hd 
                  |> int_of_string in 
    let st_date = String.split_on_char 'T' st_str |> List.hd |> int_of_string  in 
    let end_date = String.split_on_char 'T' end_str |> List.hd |> int_of_string in 
    let syear = st_date /10000 in 
    let srest = st_date mod 10000 in 
    let smonth = srest / 100 in 
    let sday = srest mod 100 in 
    let eyear = end_date/10000 in 
    let erest = end_date mod 10000 in 
    let emonth = erest / 100 in 
    let eday = erest mod 100 in 
    create_calendar_event name st_time sday smonth syear ed_time eday emonth eyear
  else raise Not_found

(** [line] is a reference to the current line being read from the calender file. *)
let line = ref ""

(** [get_name ic] sets the value of [line]. *)
let get_name ic = 
  line := input_line ic; 
  while not (contains "SUMMARY:" !line) do
    line := input_line ic 
  done

(** [return_name ic] is the name of the event. *)
let return_name ic = 
  get_name ic; 
  Str.string_after !line 8 

(** [end_info] is a reference to the input containing information about end time 
    and date of the current event. *)
let end_info = ref ""

(**  [get_end ic] sets the value of [end_info]. *)
let get_end ic = 
  end_info := input_line ic;
  while not (contains "DTEND;" !end_info) do 
    let line = input_line ic in 
    end_info := line
  done 

(** [return_end ic] is the end date and time of the current event. *)
let return_end ic = 
  get_end ic; !end_info

(** [start_info] is a reference to the input containing information about the 
    start time and date of the current event. *)
let start_info = ref ""

(** [get_start ic] sets the value of [start_info]. *)
let get_start ic = 
  start_info := input_line ic;
  while not (contains "DTSTART;" !start_info) do 
    start_info := input_line ic
  done 

(** [return_start ic] is the start date and time of the current event. *)
let return_start ic = 
  get_start ic; !start_info

(** [return_events file] stores all the events from the [file] in [events]. *) 
let return_events file = 
  let ic = open_in file in 
  (try
     while true do
       let l = input_line ic in
       let cond = (contains "BEGIN" l && contains "EVENT" l) in 
       (if cond then 
          let ed = return_end ic in 
          let name = return_name ic in 
          let st = return_start ic in 
          ignore(events := (extract_event st ed name)::(!events));
        else ())
     done;
   with End_of_file -> 
     close_in ic;)

(** [time_compare time_a day_a month_a year_a time_b day_b month_b year_b] is 2 
    if years, dates and times are the same. Otherwise, it is Int.compare [a] [b]
    where a and b are differing attributes between the two events. *)
let time_compare time_a day_a month_a year_a time_b day_b month_b year_b =
  if not(year_a = year_b) then Int.compare year_a year_b
  else if not(month_a = month_b) then Int.compare month_a month_b
  else if not(day_a = day_b) then Int.compare day_a day_b
  else Int.compare time_a time_b

(** [find_similar lst name stime sd sm sy etime ed em ey] is true if there are
    no events in [lst] that overlap with the requested event. *)
let rec find_similar lst name stime sd sm sy etime ed em ey = 
  match lst with 
  |  [] -> true
  | ({name = n; start_time = st; start_day = sday;
      start_month = smonth; start_year = syear; end_time = et; 
      end_day = eday; end_month = emonth; end_year = eyear})::t -> 
    let cmp_sa_eb = time_compare stime sd sm sy et eday emonth eyear in
    let cmp_sb_ea = time_compare etime ed em ey st sday smonth syear in
    if (not((cmp_sa_eb = 1) || (cmp_sb_ea = -1) || (cmp_sa_eb = 0) || (cmp_sb_ea = 0))) then 
      false
    else find_similar t name stime sd sm sy etime ed em ey

let add_to_calendar file name stime sd sm sy etime ed em ey = 
  return_events file; (* populates the events reference *)
  let lst = !events in 
  find_similar lst name stime sd sm sy etime ed em ey 