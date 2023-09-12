open OCanren
open OCanren.Std

let insert_window q first second third fourth day =
  match q with
  | 1 -> first === !!(-1)
  | 2 -> second === !!(-1)
  | 3 -> third === !!(-1)
  | 4 -> fourth === !!(-1)
  | 5 -> day === Std.list Fun.id [ !!(-1); !!(-1); !!(-1); !!(-1) ]
  | _ -> success
;;

let delete_monday q group =
  fresh
    (a1 b1 b2 b3 b4)
    (group === Std.list Fun.id [ a1; __; __; __; __ ])
    (a1 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a1)
;;

let delete_tuesday q group =
  fresh
    (a2 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; a2; __; __; __ ])
    (a2 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a2)
;;

let delete_wednesday q group =
  fresh
    (a3 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; __; a3; __; __ ])
    (a3 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a3)
;;

let delete_thursday q group =
  fresh
    (a4 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; __; __; a4; __ ])
    (a4 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a4)
;;

let delete_friday q group =
  fresh
    (a5 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; __; __; __; a5 ])
    (a5 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a5)
;;

let rec myassoco key xs v =
  condeep
    [ xs === List.nil () &&&& failure
    ; Fresh.three (fun a b tl ->
        xs
        === Std.pair a b % tl
        &&&& condeep [ a === key &&&& (b === v); a =/= key &&&& myassoco key tl v ])
    ]
;;

let delete_day n q group =
  match n with
  | 1 -> delete_monday q group
  | 2 -> delete_tuesday q group
  | 3 -> delete_wednesday q group
  | 4 -> delete_thursday q group
  | 5 -> delete_friday q group
  | _ -> success
;;

let rec use_constraint storage list_pair_constraint =
  match list_pair_constraint with
  | [] -> success
  | hd :: tl ->
    fresh
      (group_name sched)
      (match hd with
       | [ hd1; hd2; tl ] ->
         group_name
         === !!hd1
         &&&& myassoco group_name storage sched
         &&&& delete_day hd2 tl sched
       | _ -> success)
      (use_constraint storage tl)
;;

let free_lesson_for_second_group group1 group2 teacher subj =
  let open Init_core in
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5 d1 d2 d3 d4 d5 class_sched)
    (group1 === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (class_sched === Std.list Fun.id [ b1; b2; b3; b4; b5 ])
    (teacher === Std.list Fun.id [ c1; c2; c3; c4; c5 ])
    (group2 === Std.list Fun.id [ d1; d2; d3; d4; d5 ])
    (condeep
       [ insert_lesson_to_second_session subj a1 b1 c1 &&&& delete_monday 2 group2
       ; insert_lesson_to_third_session subj a1 b1 c1 &&&& delete_monday 3 group2
       ; insert_lesson_to_second_session subj a2 b2 c2 &&&& delete_tuesday 2 group2
       ; insert_lesson_to_third_session subj a2 b2 c2 &&&& delete_tuesday 3 group2
       ; insert_lesson_to_second_session subj a3 b3 c3 &&&& delete_wednesday 2 group2
       ; insert_lesson_to_third_session subj a3 b3 c3 &&&& delete_wednesday 3 group2
       ; insert_lesson_to_second_session subj a4 b4 c4 &&&& delete_thursday 2 group2
       ; insert_lesson_to_third_session subj a4 b4 c4 &&&& delete_thursday 3 group2
       ; insert_lesson_to_second_session subj a5 b5 c5 &&&& delete_friday 2 group2
       ; insert_lesson_to_third_session subj a5 b5 c5 &&&& delete_friday 3 group2
       ; insert_lesson_to_fourth_session subj a5 b5 c5 &&&& delete_friday 4 group2
       ; insert_lesson_to_first_session subj a1 b1 c1 &&&& delete_monday 1 group2
       ; insert_lesson_to_fourth_session subj a1 b1 c1 &&&& delete_monday 4 group2
       ; insert_lesson_to_first_session subj a2 b2 c2 &&&& delete_tuesday 1 group2
       ; insert_lesson_to_fourth_session subj a2 b2 c2 &&&& delete_tuesday 4 group2
       ; insert_lesson_to_first_session subj a3 b3 c3 &&&& delete_wednesday 1 group2
       ; insert_lesson_to_fourth_session subj a3 b3 c3 &&&& delete_wednesday 4 group2
       ; insert_lesson_to_first_session subj a4 b4 c4 &&&& delete_thursday 1 group2
       ; insert_lesson_to_fourth_session subj a4 b4 c4 &&&& delete_thursday 4 group2
       ; insert_lesson_to_first_session subj a5 b5 c5 &&&& delete_friday 2 group2
       ])
;;

let rec use_no_formal_constraints storage (list_const : int list list) =
  match list_const with
  | [] -> success
  | hd :: tl ->
    fresh
      (group_name_1 group_name_2 sched_1 sched_2 teacher_sched subj_rel teacher_name)
      (match hd with
       | [ group1; group2; teacher; subj ] ->
         group_name_1
         === !!group1
         &&&& (group_name_2 === !!group2)
         &&&& (teacher_name === !!teacher)
         &&&& (subj_rel === !!subj)
         &&&& myassoco group_name_1 storage sched_1
         &&&& myassoco group_name_2 storage sched_2
         &&&& myassoco teacher_name storage teacher_sched
         &&&& free_lesson_for_second_group sched_1 sched_2 teacher_sched subj_rel
       | _ -> success)
      (use_no_formal_constraints storage tl)
;;
