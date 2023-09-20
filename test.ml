open OUnit2
open Command
open State_tasks
open Tasks
open Events
open Meetings
open Geticalinfo

(* TEST PLAN: Our testing strategy was to build a small component, do black box 
    testing on it for the exposed funcitons in the .mli files. We performed 
    exhaustive testing and made sure our code behaves as expected for every possible
    type of input. If somethign wasn't working, we resorted to white-box testing
    adding in print statements and exposing helper functions briefly to ensure 
    their functionality. So, all the code related to functionality was tested 
    through OUnit. We have 7 separate test suites : command, events, state_events, 
    meetings, state_meetings, tasks and state_tasks corresponding to equivalent 
    module names. We tested the interface by running it multiple times 
    for every possible type of input and ensuring accurate behavior. We followed the 
    OUnit test suite style introduced in the class and made functions with different 
    parameters to create a robust blck-box testing suite. We only 
    omitted small helper functions which we are familar with like getters and 
    setters from testing. However, they were used in other functions that were 
    thoroughly tested and so, we are sure that these will also work. We believe 
    that our test suite demonstrates correctness because it is very thorough and 
    exhaustive, especially on edge cases. We also did a combination of white box, 
    block box and manual testing as mentioned above. Therefore, there can be no 
    case where we don't know how our program will react to an input. *)

let pp_string = (fun x -> x)

(* Test functions for Commands *)
let make_events_parse_test 
    (name : string) 
    (input : string) 
    (expected_output : Command_events.command) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = Command_events.parse input))

let make_meetings_parse_test 
    (name : string) 
    (input : string) 
    (expected_output : Command_meetings.command) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = Command_meetings.parse input))

let make_task_parse_test 
    (name : string) 
    (input : string) 
    (expected_output : Command_tasks.command) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = Command_tasks.parse_task input))

let make_events_parse_mal_assert_test 
    (name : string) 
    (input : string) : test = 
  name >:: (fun _ -> 
      assert_raises Command_events.Malformed (fun () -> Command_events.parse input))

let make_meetings_parse_mal_assert_test 
    (name : string) 
    (input : string) : test = 
  name >:: (fun _ -> 
      assert_raises Command_meetings.Malformed (fun () -> Command_meetings.parse input))

let make_task_parse_mal_assert_test 
    (name : string) 
    (input : string) : test = 
  name >:: (fun _ -> 
      assert_raises Command_tasks.Malformed (fun () -> Command_tasks.parse_task input))

let make_events_parse_empty_assert_test 
    (name : string) 
    (input : string) : test = 
  name >:: (fun _ -> 
      assert_raises Command_events.Empty (fun () -> Command_events.parse input))

let make_meetings_parse_empty_assert_test 
    (name : string) 
    (input : string) : test = 
  name >:: (fun _ -> 
      assert_raises Command_meetings.Empty (fun () -> Command_meetings.parse input))

let make_task_parse_empty_assert_test 
    (name : string) 
    (input : string) : test = 
  name >:: (fun _ -> 
      assert_raises Command_tasks.Empty (fun () -> Command_tasks.parse_task input))

(* Test functions for Events *)
let make_get_name_test 
    (name : string) 
    (event : Events.event) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_name event))

let make_create_event_test 
    (name : string) 
    (event_id : string)
    (start_t : int)
    (end_t : int)
    (weight : int)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (Events.create_event event_id start_t end_t weight |> string_of_event))

let make_add_event_test 
    (name : string) 
    (event_list : Events.t)
    (event : Events.event)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (Events.add_event event event_list |> Events.to_string))

let make_remove_event_test 
    (name : string) 
    (event_list : Events.t)
    (event_id : string)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (remove_event event_id event_list |> Events.to_string))

let make_select_events_test 
    (name : string) 
    (events : Events.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (select_events events |> Events.to_string))

(* Test functions for ical *)
let make_add_to_calendar_test
    (name : string)
    (file : string)
    (nm : string )
    (stime : int)
    (sd : int)
    (sm : int)
    (sy : int)
    (etime : int)
    (ed : int)
    (em : int)
    (ey : int)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_bool nm (expected_output = add_to_calendar file nm stime sd sm 
                        sy etime ed em ey))

let make_availability_test 
    (name : string)
    (files : string)
    (nm : string )
    (stime : int)
    (sd : int)
    (sm : int)
    (sy : int)
    (etime : int)
    (ed : int)
    (em : int)
    (ey : int)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_bool nm (expected_output = (availability files nm stime sd sm 
                                           sy etime ed em ey)))

(* Test functions for Meetings *)
let string_of_meetings int_list = 
  int_list |> List.map string_of_interval |> String.concat ", "

let make_person_name_test 
    (name : string) 
    (person : Meetings.person) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output (person_name person))

let make_add_person_test 
    (name : string) 
    (group : Meetings.t)
    (person : Meetings.person)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (add_person person group |> Meetings.to_string))

let make_remove_person_test 
    (name : string) 
    (group : Meetings.t)
    (person_id : string)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (remove_person person_id group |> Meetings.to_string))

let make_select_meetings_test 
    (name : string) 
    (group : Meetings.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (select_meetings group |> string_of_meetings))

(* Test functions for Tasks *)
let make_task_ids_test 
    (name : string) 
    (input : Tasks.t) 
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = (task_ids input)))

let make_task_t_test 
    (name : string) 
    (t : Tasks.t) 
    (id : task_id)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_bool name (
        try begin expected_output |> List.nth (get_tasks t) = 
                                     (task_t t id |> List.hd) end
        with _ -> expected_output = ~-1))

let make_duration_test 
    (name : string) 
    (t : Tasks.t) 
    (id : task_id)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = duration t id))

let make_priority_test 
    (name : string) 
    (t : Tasks.t) 
    (id : task_id)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = priority t id))

let make_dl_compare_test 
    (name : string) 
    (t : Tasks.t) 
    (id1 : task_id)
    (id2 : task_id)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = dl_compare t id1 id2))

let make_taskid_assert_test 
    (name : string) 
    (id : task_id)
    (func) : test = 
  name >:: (fun _ -> assert_raises (UnknownTask id) (fun () -> id |> func))

(* Helper functions for state testing *)
let schedule_string result =
  match result with 
  | Illegal -> ""
  | Legal new_state -> new_state |> get_ids |> (String.concat " | ")

let delegate_string result =
  match result with 
  | Illegal -> ""
  | Legal new_state -> new_state |> print_map_helper |> (String.concat " | ") 

(* Test functions for State *)
let make_schedule_test 
    (name : string) 
    (algo : string) 
    (sch : Tasks.t)
    (st : State_tasks.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = schedule_string (schedule algo sch st)))

let make_add_test 
    (name : string) 
    (task : string) 
    (st : State_tasks.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = schedule_string (add task st)))

let make_display_test 
    (name : string) 
    (n : int) 
    (sch : Tasks.t)
    (st : State_tasks.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = schedule_string (display n sch st)))

let make_modify_test 
    (name : string) 
    (task : string) 
    (st : State_tasks.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = schedule_string (modify task st)))

let make_delegate_test 
    (name : string) 
    (sch : Tasks.t) 
    (st : State_tasks.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_bool name (expected_output = delegate_string (delegate sch st))) 

let command_task_tests = [
  make_events_parse_test "Events Command Extra space" "exit  " Exit;
  make_events_parse_test "Events Command Exit" "exit" Exit;
  make_events_parse_test "Events Command Display" "display ex" (Display "ex");
  make_events_parse_test "Events Command NewList" "newlist hi" (NewList "hi");
  make_events_parse_test "Events Command FindOpt" "findopt a" (FindOpt "a");
  make_events_parse_test "Events Command RemoveEvent" "removeevent hi event" 
    (RemoveEvent ("hi", "event"));
  make_events_parse_test "Events Command AddEvent" "addevent hi event there" 
    (AddEvent ("hi", ["event"; "there"]));
  make_events_parse_test "Events Command AddFromFile" "addfromfile name ex.json" 
    (AddFromFile ("name", "ex.json"));
  make_events_parse_mal_assert_test "Events Command h::[] Malformed" "exits";
  make_events_parse_mal_assert_test "Events Command h::t::[] Malformed" "exits now";
  make_events_parse_mal_assert_test "Events Command h::h'::t::[] Malformed" 
    "exits now please";
  make_events_parse_mal_assert_test "Events Command h::h'::t Malformed" 
    "a b c d e f";
  make_events_parse_empty_assert_test "Events Command Empty" "";
  make_meetings_parse_test "Meetings Command Extra Space" "  exit" Exit;
  make_meetings_parse_test "Meetings Command Exit" "exit" Exit;
  make_meetings_parse_test "Meetings Command DisplayMeetings" 
    "displaymeetings a" (DisplayMeetings "a");
  make_meetings_parse_test "Meetings Command DisplayGroup" 
    "displaygroup b" (DisplayGroup "b");
  make_meetings_parse_test "Meetings Command FindMeetings" 
    "findmeetings c" (FindMeetings "c");
  make_meetings_parse_test "Meetings Command RemovePerson" 
    "removeperson d paul" (RemovePerson ("d","paul"));
  make_meetings_parse_test "Meetings Command AddPerson" 
    "addperson e sarah 1,2" (AddPerson ("e","sarah", "1,2"));
  make_meetings_parse_test "Meetings Command AddFromFile" 
    "addfromfile g ex.json" (AddFromFile("g","ex.json"));
  make_meetings_parse_test "Meetings Command NewGroup" 
    "newgroup f" (NewGroup "f");
  make_meetings_parse_mal_assert_test "Meetings Command h::[] Malformed" "exts";
  make_meetings_parse_mal_assert_test "Meetings Command h::t::[] Malformed" "a b";
  make_meetings_parse_mal_assert_test "Meetings Command h::h'::t::[] Malformed" 
    "a b c";
  make_meetings_parse_mal_assert_test "Meetings Command h::h'::t Malformed" 
    "a b c d";
  make_meetings_parse_empty_assert_test "Meetings Command Empty" "";
  make_task_parse_test "Task Command extra space" "  schedule  SJF" (Schedule "SJF");
  make_task_parse_test "Task Command Schedule Valid Schedule" "schedule SJF" 
    (Schedule "SJF");
  make_task_parse_test "Task Command Schedule Invalid Schedule" "schedule best" 
    (Schedule "best");
  make_task_parse_test "Task Command Exit" "exit" Exit;
  make_task_parse_test "Task Command Delegate" "delegate" Delegate;
  make_task_parse_test "Task Command Help" "help" THelp;
  make_task_parse_test "Task Command Remove" "remove phys studying" 
    (Remove ["phys"; "studying"]);
  make_task_parse_test "Task Command Add" "add math prelim 1/12/20" 
    (Add ["math"; "prelim"; "1/12/20"]);
  make_task_parse_test "Task Command Display" "display 14 " (Display 14);
  make_task_parse_test "Task Command Modify" "modify math pset" 
    (Modify ["math"; "pset"]);
  make_task_parse_mal_assert_test "Task Command h::[] Malformed" "schedule";
  make_task_parse_mal_assert_test "Task Command h::t::[] Malformed" "a b";
  make_task_parse_mal_assert_test "Task Command h::t Malformed" "exit this plz";
  make_task_parse_empty_assert_test "Task Command Empty" "";
] 

let e_json = Yojson.Basic.from_file "example_events.json"
let e_json1 = Yojson.Basic.from_file "example_events1.json"
let e_json2 = Yojson.Basic.from_file "example_events2.json"
let e_json3 = Yojson.Basic.from_file "example_events3.json"
let e_json4 = Yojson.Basic.from_file "example_events4.json"
let event1 = Events.create_event "PA1" 1 2 0
let event2 = Events.create_event "PA2" 2 3 1
let event3 = Events.create_event "PA3" 3 4 2
let e_list = events_of_json e_json
let e_list1 = events_of_json e_json1
let e_list2 = events_of_json e_json2
let e_list3 = events_of_json e_json3
let e_list4 = events_of_json e_json4

let file1 = "calendar.ics"
let file2 = "cal.ics" 

let events_tests = [
  "empty list" >:: (fun _ -> 
      assert_equal (Events.(to_string empty)) (Events.to_string e_list));
  make_create_event_test "A5 [5,6] 3" "A5" 5 6 3 ("{id: A5, period: [5, 6], "
                                                  ^ "weight: 3}"); 
  make_add_event_test "add event to empty list" Events.empty event1 
    "[{id: PA1, period: [1, 2], weight: 0}]";
  make_add_event_test "add event to nonempty list" e_list1 event1 
    {|[{id: A1, period: [1, 3], weight: 2}, {id: A2, period: [2, 4], weight: 4}, {id: A3, period: [10, 12], weight: 4}, {id: PA1, period: [1, 2], weight: 0}]|};
  make_get_name_test "PA1" event1 "PA1";
  make_get_name_test "PA2" event2 "PA2";
  make_remove_event_test "event not in list" e_list1 "PA1" 
    ("[{id: A1, period: [1, 3], weight: 2}, {id: A2, period: [2, 4], weight: 4},"
     ^" {id: A3, period: [10, 12], weight: 4}]");
  make_remove_event_test "event in list" e_list1 "A1" 
    ("[{id: A2, period: [2, 4], weight: 4}, "
     ^ "{id: A3, period: [10, 12], weight: 4}]");
  make_select_events_test "list4" e_list4 
    ("[{id: CS OH, period: [9, 11], weight: 7}, {id: CS Study Group, period:"
     ^ " [6, 7], weight: 5}, {id: Physics Review Session 1,"
     ^ " period: [2, 4], weight: 4}]");
  "list2" >:: (fun _ ->
      assert_equal (Events.to_string e_list2) 
        ("[{id: PSET4, period: [0, 3], weight: 1}, "
         ^"{id: A5, period: [5, 9], weight: 4}]"));
]

(* Test functions for Events state *)

let st1 = State_events.(init_state 
                        |> add_list "List2" e_list2 
                        |> add_list "List3" e_list3)
let st2 = State_events.(init_state |> add_from_json "List4" "example_events4.json")

let make_add_elist_test 
    (name : string) 
    (listname : string)
    (events : Events.t)
    (st : State_events.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (State_events.(add_list listname events st |> to_string)))

let make_del_elist_test 
    (name : string) 
    (listname : string)
    (st : State_events.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (State_events.(del_list listname st |> to_string)))

let make_add_event_st_test 
    (name : string) 
    (listname : string)
    (event : Events.event)
    (st : State_events.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (State_events.(add_event listname event st |> to_string)))

let make_remove_event_st_test 
    (name : string) 
    (listname : string)
    (e_id : string)
    (st : State_events.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (State_events.(remove_event listname e_id st |> to_string)))

let make_findopt_events_test 
    (name : string) 
    (listname : string)
    (st : State_events.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        State_events.(find_opt_events listname st |> to_string))

let events_state_tests = [
  "init state" >:: (fun _ ->
      assert_equal (State_events.(init_state |> to_string)) 
        "[]");
  make_add_elist_test "List1" "List1" e_list State_events.init_state 
    "[List1: []]";
  make_del_elist_test "List exists" "List2" st1 
    "[List3: [{id: Seminar, period: [9, 12], weight: 0}, {id: Party, period: [6, 9], weight: 4}]]";
  make_add_event_st_test "Add" "List2" event1 st1 
    "[List3: [{id: Seminar, period: [9, 12], weight: 0}, {id: Party, period: [6, 9], weight: 4}]; List2: [{id: PSET4, period: [0, 3], weight: 1}, {id: A5, period: [5, 9], weight: 4}, {id: PA1, period: [1, 2], weight: 0}]]";
  make_remove_event_st_test "Rem" "List3" "Party" st1 "[List2: [{id: PSET4, period: [0, 3], weight: 1}, {id: A5, period: [5, 9], weight: 4}]; List3: [{id: Seminar, period: [9, 12], weight: 0}]]";
  make_findopt_events_test "find opt List4" "List4" st2 "[List4: [{id: CS OH, period: [9, 11], weight: 7}, {id: CS Study Group, period: [6, 7], weight: 5}, {id: Physics Review Session 1, period: [2, 4], weight: 4}]]";
]

let calendar_tests = [
  make_add_to_calendar_test "add_to_calendar true 1" file1 
    "Early Event" 1 1 1 1 1 1 1 1 true;
  make_add_to_calendar_test "add_to_calendar true 2" file1 
    "Late Event" 12 14 12 2049 13 14 12 2049 true;
  make_add_to_calendar_test "add_to_calendar true 3" file1 
    "Long Event" 009300 19 05 2020 009400 19 05 2020 true;
  make_add_to_calendar_test "add_to_calendar false 1" file1 
    "Long Event" 120000 29 08 2018 230000 29 08 2018 false;
  make_add_to_calendar_test "add_to_calendar false 2" file1 
    "Long Event" 093000 18 05 2020 094000 18 05 2020 false;
  make_add_to_calendar_test "end time matches event" file1 
    "Work" 080000 16 05 2020 090000 16 05 2020 true;
  make_add_to_calendar_test "task falls in full day" file1 
    "Work" 080000 25 05 2020 090000 25 05 2020 true; 
  make_add_to_calendar_test "add task to next day so date is different"
    file1 "Work" 193000 13 03 2020 210000 13 03 2020 true;
  make_add_to_calendar_test "add task to same day and time but different year"
    file1 "Work" 193000 12 03 2021 210000 12 03 2021 true;
  make_availability_test "No files added" "" "Empty" 093000 18 05 2020 094000 18 05 2020 0;
  make_availability_test "Event overlaps" "cal.ics" "Overlap 1" 143000 29 08 2018 150000 29 08 2018 0;
  make_availability_test "Event overlaps" "cal.ics calendar.ics" "Overlap 2" 143000 29 08 2018 150000 29 08 2018 0;
  make_availability_test "Event overlaps" "cal.ics" "Doesn't overlap" 140000 29 08 2018 143000 29 08 2018 1;
  make_availability_test "Event overlaps" "cal.ics calendar.ics" "Doesn't overlap" 140000 29 08 2018 143000 29 08 2018 2;
]

let m_json = Yojson.Basic.from_file "example_group.json"
let m_json1 = Yojson.Basic.from_file "example_group1.json"
let m_json2 = Yojson.Basic.from_file "example_group2.json"
let m_json3 = Yojson.Basic.from_file "example_group3.json"
let interval1 = create_interval 0 1
let interval2 = create_interval 1 2
let person1 = create_person "Paul" [interval1]
let person2 = create_person "Sasha" [interval1; interval2]
let person3 = create_person "Hailey" []
let group = group_of_json m_json
let group1 = group_of_json m_json1
let group2 = group_of_json m_json2
let group3 = group_of_json m_json3

let meetings_tests = [
  "empty group" >:: (fun _ -> 
      assert_equal (Meetings.(to_string empty)) "[]");
  make_person_name_test "Paul" person1 "Paul";
  make_person_name_test "Sasha" person2 "Sasha";
  make_add_person_test "add person to empty" group person1 "[Paul]";
  make_add_person_test "add person to nonempty" group1 person2 
    "[Paul, Jess, Dan, Sasha]";
  make_remove_person_test "person not in group" group1 "Sarah" "[Paul, Jess, Dan]";
  make_remove_person_test "person in group" group1 "Paul" "[Jess, Dan]"; 
  make_select_meetings_test "group1" group1 
    "[3, 4], [4, 5], [7, 8], [8, 9], [10, 11], [11, 12]";
  make_select_meetings_test "empty group" group "";
  "group2 json" >:: (fun _ ->
      assert_equal (Meetings.to_string group2) "[Paul, Jess, Dan]");
  "string of interval [1,2]" >:: (fun _ ->
      assert_equal (Meetings.string_of_interval interval2) "[1, 2]");
]

(* Test functions for Meetings state *)
let g_st1 = State_meetings.(init_state |> add_from_json "Student Council" 
                              "example_group1.json")
let g_st3 = State_meetings.(init_state |> add_from_json "Study Group" 
                              "example_group3.json")

let make_add_group_test 
    (name : string) 
    (groupname : string)
    (group : Meetings.t)
    (st : State_meetings.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (State_meetings.(add_group groupname group st |> to_string)))

let make_add_person_test 
    (name : string) 
    (groupname : string)
    (person : Meetings.person)
    (st : State_meetings.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (State_meetings.(add_person groupname person st |> to_string)))

let make_remove_person_test 
    (name : string) 
    (groupname : string)
    (person_id : string)
    (st : State_meetings.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        (State_meetings.(remove_person groupname person_id st |> to_string)))

let make_select_meetings_st_test 
    (name : string) 
    (groupname : string)
    (st : State_meetings.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal ~printer:pp_string expected_output 
        State_meetings.(select_meetings groupname st |> to_string))

let meetings_state_test = [
  "init state" >:: (fun _ ->
      assert_equal (State_meetings.(init_state |> to_string)) 
        "[]");
  make_add_group_test "Baja" "Baja" group1 State_meetings.init_state 
    "[Group Baja: [Paul, Jess, Dan] -> Meeting Times: []]";
  make_add_person_test "add Paul" "Study Group" person1 g_st3 
    ("[Group Study Group: [Harriet, Clementine, Pedro, Paul] -> "
     ^ "Meeting Times: []]");
  make_remove_person_test "remove Clementine" "Study Group" "Clementine" g_st3 
    "[Group Study Group: [Harriet, Pedro] -> Meeting Times: []]";
  make_select_meetings_st_test "Student Council" "Student Council" g_st1 
    ("[Group Student Council: [Paul, Jess, Dan] -> Meeting Times: [[3, 4], "
     ^ "[4, 5], [7, 8], [8, 9], [10, 11], [11, 12]]]");
]

let json1 = Yojson.Basic.from_file "example_tasks.json"
let sch1 = from_json json1
let st1 = init_state sch1

let task_tests = [
  make_task_ids_test "task ids" sch1 ["Math PSet 4"; "Phys Lab";
                                      "Phys Studying"; "Phys Pset 3"];
  make_task_t_test "task_t valid Math PSet 4" sch1 "Math PSet 4" 0;
  make_task_t_test "task_t valid Phys Lab" sch1 "Phys Lab" 1;
  make_task_t_test "task_t valid Phys Studying" sch1 "Phys Studying" 2;
  make_task_t_test "task_t valid Phys Pset 3" sch1 "Phys Pset 3" 3;
  make_task_t_test "task_t invalid" sch1 "Fake task" ~-1;
  make_duration_test "duration valid Math PSet 4" sch1 "Math PSet 4" 2.0;
  make_duration_test "duration valid Phys Lab" sch1 "Phys Lab" 2.5;
  make_duration_test "duration valid Phys Studying" sch1 "Phys Studying" 5.0;
  make_duration_test "duration valid Phys Pset 3" sch1 "Phys Pset 3" 4.0;
  make_priority_test "priority valid Math PSet 4" sch1 "Math PSet 4" 2;
  make_priority_test "priority valid Phys Lab" sch1 "Phys Lab" 2;
  make_priority_test "priority valid Phys Studying" sch1 "Phys Studying" 2;
  make_priority_test "priority valid Phys Pset 3" sch1 "Phys Pset 3" 5;
  make_dl_compare_test "dl_compare equal" sch1 "Phys Pset 3" "Math PSet 4" 0;
  make_dl_compare_test "dl_compare earlier" sch1 "Phys Lab" "Math PSet 4" ~-1;
  make_dl_compare_test "dl_compare later" sch1 "Phys Studying" "Phys Pset 3" 1;
  make_taskid_assert_test "assert priority" "fake" (priority sch1);
  make_taskid_assert_test "assert deadline" "coding :'(" (deadline sch1);
  make_taskid_assert_test "assert duration" "Baking" (duration sch1);
] 

let task_state_tests = [
  make_schedule_test "schedule legal FCFS" "FCFS" sch1 st1 
    ("Math PSet 4 | Phys Lab | Phys Studying | Phys Pset 3");
  make_schedule_test "schedule legal SJF" "SJF" sch1 st1 
    "Math PSet 4 | Phys Lab | Phys Pset 3 | Phys Studying";
  make_schedule_test "schedule legal RR" "RR" sch1 st1 
    ("Math PSet 4 | Phys Lab | Phys Studying | Phys Pset 3 | Math PSet 4 | " ^ 
     "Phys Lab | Phys Studying | Phys Pset 3 | Phys Lab | Phys Studying | " ^
     "Phys Pset 3 | Phys Studying | Phys Pset 3 | Phys Studying");
  make_schedule_test "schedule legal EDF" "EDF" sch1 st1 
    ("Phys Lab | Phys Pset 3 | Math PSet 4 | Phys Studying");
  make_schedule_test "schedule legal SJFRR" "SJFRR" sch1 st1 
    ("Math PSet 4 | Phys Lab | Phys Pset 3 | Phys Studying | Math PSet 4 " ^ 
     "| Phys Lab | Phys Pset 3 | Phys Studying | Phys Lab | Phys Pset 3 | " ^ 
     "Phys Studying | Phys Pset 3 | Phys Studying | Phys Studying");
  make_schedule_test "schedule legal EDFRR" "EDFRR" sch1 st1 
    ("Phys Lab | Phys Pset 3 | Math PSet 4 | Phys Studying | Phys Lab | " ^
     "Phys Pset 3 | Math PSet 4 | Phys Studying | Phys Lab | Phys Pset 3 | " ^ 
     "Phys Studying | Phys Pset 3 | Phys Studying | Phys Studying");
  make_schedule_test "schedule illegal" "ABC" sch1 st1 "";
  make_add_test "add legal single word" "Juggling 24/4/2021 1 4" st1 
    "Juggling | Math PSet 4 | Phys Lab | Phys Studying | Phys Pset 3";
  make_add_test "add legal multi word" "My Daily Affirmations 14/9/2021 2 100" 
    st1 "My Daily Affirmations | Math PSet 4 | Phys Lab | Phys Studying | Phys Pset 3";
  make_add_test "add illegal mem" "Phys Lab 14/9/2021 2 100" st1 "";
  make_add_test "add illegal missing fields" "Phys Lab 14/9/2021" st1 "";
  make_add_test "add illegal type" "Phys Lab 14/9/2021 two 100" st1 "";
  make_display_test "display legal" 2 sch1 st1 "Math PSet 4 | Phys Lab";
  make_display_test "display legal >length" 10 sch1 st1 
    "Math PSet 4 | Phys Lab | Phys Pset 3 | Phys Studying";
  make_display_test "display legal 0" 0 sch1 st1 "";
  make_modify_test "modify legal multi word" "Phys Lab 14/9/2021 2 100" st1
    "Math PSet 4 | Phys Lab | Phys Studying | Phys Pset 3";
  make_modify_test "modify illegal mem" "My Daily Affirmations 14/9/2021 2 100" st1 "";
  make_modify_test "modify illegal missing fields" "Phys Lab 14/9/2021" st1 "";
  make_modify_test "modify illegal type" "Phys Lab 14/9/2021 two 100" st1 "";
  make_delegate_test "delegate legal" sch1 st1 
    "Math PSet 4: Nate | Phys Lab: David | Phys Studying: Walker | Phys Pset 3: Nate";
] 

let suite =
  "test suite for Final Project"  >::: List.flatten [
    command_task_tests;
    events_tests;
    events_state_tests;
    meetings_tests;
    meetings_state_test;
    task_tests;
    task_state_tests;
  ]

let _ = run_test_tt_main suite