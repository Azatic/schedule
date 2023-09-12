val func :
  string list list ->
  string list list ->
  (int OCanren.logic *
   int OCanren.logic OCanren.Std.List.logic OCanren.Std.List.logic)
  OCanren.logic OCanren.Std.List.logic -> (string * string list list) list
val schedo :
  string list list ->
  string list list -> string list list -> string list list -> unit
  (** schedo is function this is a function that accepts as input 1) a list of constraints,
     2) a list of pairs,
      3) a list of lectures and 
      4) a list of informal constraints in the form of arguments
      

      
      1) The first argument is a list of arbitrary length consisting of lists of length 3,
       where the first element is the name of the group, the second is the day of the week,
       the third is the number of the class. This group has a window on this pair on this day 
       2) The second argument has the form 
       [[group_name ; teacher_name; subj_name] ; ... ; [group_name ; teacher_name; subj_name]]
        this argument is responsible for inserting pairs 
        3) the third argument has the form
         [[ Name_1group; Name_2group; Name_3group; Name_4group; Teacher_name; Discipline_name ] ; ... ;
          [ Name_1group; Name_2group; Name_3group; Name_4group; Teacher_name; Discipline_name ]]
           this argument is responsible for inserting Lectire
           
           4) ... *)
val t : float
