module M = Author

let () = 
  if M.hours_worked < 0
  then print_string
      "===========================================================\n\
       WARNING: \n\
       You have not set hours_worked. Please read the submission\n\
       instructions in the assignment handout.\n\
       ===========================================================\n"
