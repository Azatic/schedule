open OCanren
open OCanren.Std
open Constraint_core
open Init_core
open Type_core

let rec membero x l = conde [ List.caro l x; fresh d (List.cdro l d) (membero x d) ]

let insert_all_sched subj group_sched teacher_sched class_sched =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5)
    (group_sched === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (teacher_sched === Std.list Fun.id [ b1; b2; b3; b4; b5 ])
    (class_sched === Std.list Fun.id [ c1; c2; c3; c4; c5 ])
    (conde
       [ Init_core.ins_sched1 subj a1 b1 c1
       ; ins_sched2 subj a1 b1 c1
       ; ins_sched3 subj a1 b1 c1
       ; ins_sched4 subj a1 b1 c1
       ; ins_sched1 subj a2 b2 c2
       ; ins_sched2 subj a2 b2 c2
       ; ins_sched3 subj a2 b2 c2
       ; ins_sched4 subj a2 b2 c2
       ; ins_sched1 subj a3 b3 c3
       ; ins_sched2 subj a3 b3 c3
       ; ins_sched3 subj a3 b3 c3
       ; ins_sched4 subj a3 b3 c3
       ; ins_sched1 subj a4 b4 c4
       ; ins_sched2 subj a4 b4 c4
       ; ins_sched3 subj a4 b4 c4
       ; ins_sched4 subj a4 b4 c4
       ])
;;

let rec insert_sched_to_one_group
  lessons
  all_teacher_sched
  sched_all_class
  group_sched
  class_subj
  =
  conde
    [ lessons === List.nil ()
    ; fresh
        (teacher_sched
           lesson
           tail_teacher
           tail_lessons
           sched_class
           ost
           class_first
           class_ost)
        (all_teacher_sched === List.cons teacher_sched tail_teacher)
        (lessons === List.cons lesson tail_lessons)
        (sched_all_class === List.cons sched_class ost)
        (class_subj === List.cons class_first class_ost)
        (membero lesson class_first)
        (insert_all_sched lesson group_sched teacher_sched sched_class)
        (insert_sched_to_one_group
           tail_lessons
           tail_teacher
           sched_all_class
           group_sched
           class_subj)
    ; fresh
        (sched_first_class ost subj_first subj_ost)
        (sched_all_class === List.cons sched_first_class ost)
        (class_subj === List.cons subj_first subj_ost)
        (insert_sched_to_one_group lessons all_teacher_sched ost group_sched subj_ost)
    ]
;;

let rec sched studyplanallgroup schedallgroup allteachersched schedclass classessubj =
  conde
    [ studyplanallgroup === List.nil ()
    ; schedallgroup === List.nil ()
    ; fresh
        (studyplanonegroup studyplanost schedonegroup schedost oneteacher ostteacher)
        (studyplanallgroup === List.cons studyplanonegroup studyplanost)
        (schedallgroup === List.cons schedonegroup schedost)
        (allteachersched === List.cons oneteacher ostteacher)
        (insert_sched_to_one_group
           studyplanonegroup
           oneteacher
           schedclass
           schedonegroup
           classessubj)
        (sched studyplanost schedost ostteacher schedclass classessubj)
    ]
;;

[@@@ocaml.warnerror "-27"]

let rec myassoco key xs v =
  Fresh.three (fun a b tl ->
    xs
    === Std.pair a b % tl
    &&& conde
          [ a === key &&& (b === v)
          ; a =/= key &&& myassoco key tl v
          ; xs === List.nil () &&& failure
          ])
;;

let insert_sched_to_pair pair = conde [ fresh (group subj teacher) success ]

let rec insert_sched_in_storage group_name group_sched storage =
  conde
    [ fresh (a1 a2) (List.caro storage a1) (a1 === Std.pair group_name group_sched)
    ; fresh a3 (List.cdro storage a3) (insert_sched_in_storage group_name group_sched a3)
    ]
;;

let rec init_sched (list_pair : string list list) storage =
  match list_pair with
  | [] -> success
  | hd :: tl ->
    conde
      [ fresh
          (groupname teachername subjname group_sched teacher_sched aud new_storage)
          (init_sched_a_week teacher_sched)
          (init_sched_a_week group_sched)
          (init_sched_a_week aud)
          (Std.list ( !! ) hd === Std.list Fun.id [ groupname; teachername; subjname ])
          (myassoco groupname storage group_sched)
          (myassoco teachername storage (teacher_sched : ischedule))
          (insert_all_sched subjname group_sched teacher_sched aud)
          (insert_sched_in_storage groupname group_sched storage)
          (insert_sched_in_storage teachername teacher_sched storage)
          (init_sched tl storage)
      ; fresh
          (groupname teachername subjname group_sched teacher_sched aud new_storage)
          (Std.list ( !! ) hd === Std.list Fun.id [ groupname; teachername; subjname ])
          (Init_core.init_store storage)
          (init_sched_a_week teacher_sched)
          (init_sched_a_week group_sched)
          (init_sched_a_week aud)
          (insert_all_sched subjname group_sched teacher_sched aud)
          (insert_sched_in_storage groupname group_sched storage)
          (insert_sched_in_storage teachername teacher_sched storage)
          (init_sched tl storage)
      ]
;;

let rec init_sched_new (list_pair : string list list) storage n =
  match n with
  | 0 -> success
  | n ->
    init_storage n storage
    &&&
    (match list_pair with
     | [] -> success
     | hd :: tl -> init_sched list_pair storage)
;;

let rec len q =
  match q with
  | [] -> 0
  | hd :: tl -> 1 + len tl
;;

let test1 _constaints schedule answer =
  conde
    [ fresh
        storage
        (init_sched_new schedule storage (len schedule * 2))
        (use_constraint storage _constaints)
        (storage === answer)
    ]
;;
