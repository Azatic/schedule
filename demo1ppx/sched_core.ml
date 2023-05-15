open OCanren
open OCanren.Std
open Constraint_core
open Init_core
open Type_core

let rec membero x l = conde [ List.caro l x; fresh d (List.cdro l d) (membero x d) ]

let insert_all_sched_lecture subj group1 group2 group3 group4 teacher_sched class_sched =
  fresh
    (a1
       a2
       a3
       a4
       a5
       b1
       b2
       b3
       b4
       b5
       c1
       c2
       c3
       c4
       c5
       d1
       d2
       d3
       d4
       d5
       e1
       e2
       e3
       e4
       e5
       f1
       f2
       f3
       f4
       f5)
    (group1 === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (group2 === Std.list Fun.id [ d1; d2; d3; d4; d5 ])
    (group3 === Std.list Fun.id [ e1; e2; e3; e4; e5 ])
    (group4 === Std.list Fun.id [ f1; f2; f3; f4; f5 ])
    (teacher_sched === Std.list Fun.id [ b1; b2; b3; b4; b5 ])
    (class_sched === Std.list Fun.id [ c1; c2; c3; c4; c5 ])
    (conde
       [ Init_core.ins_lecture1 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture2 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture3 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture4 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture1 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture2 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture3 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture4 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture1 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture2 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture3 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture4 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture1 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture2 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture3 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture4 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture1 subj a5 d5 e5 f5 b5 c5
       ; ins_lecture2 subj a5 d5 e5 f5 b5 c5
       ; ins_lecture3 subj a5 d5 e5 f5 b5 c5
       ; ins_lecture4 subj a5 d5 e5 f5 b5 c5
       ])
;;

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
       ; ins_sched1 subj a5 b5 c5
       ; ins_sched2 subj a5 b5 c5
       ; ins_sched3 subj a5 b5 c5
       ; ins_sched4 subj a5 b5 c5
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

let rec new_assoco key xs v =
  fresh
    (a b tl all para)
    (init_sched_a_week b)
    (para === Std.list Fun.id [ a; b ])
    (appendo para tl xs)
    (conde
       [ a === key &&& (b === v)
       ; a =/= key &&& new_assoco key tl v
       ; xs === List.nil () &&& failure
       ])
;;

let insert_sched_to_pair pair = conde [ fresh (group subj teacher) success ]

let rec insert_sched_in_storage group_name group_sched storage =
  conde
    [ fresh a1 (myassoco group_name storage a1) (a1 === group_sched)
    ; fresh (a1 a2) (List.caro storage a1) (a1 === Std.pair group_name group_sched)
    ; fresh a3 (List.cdro storage a3) (insert_sched_in_storage group_name group_sched a3)
    ]
;;

let rec init_sched (list_pair : string list list) storage =
  match list_pair with
  | [] -> success
  | hd :: tl ->
    fresh
      (groupname teachername subjname group_sched teacher_sched aud new_storage)
      (init_sched_a_week teacher_sched)
      (init_sched_a_week group_sched)
      (init_sched_a_week aud)
      (Std.list ( !! ) hd === Std.list Fun.id [ groupname; teachername; subjname ])
      (myassoco groupname storage group_sched)
      (myassoco teachername storage (teacher_sched : ischedule))
      (insert_all_sched subjname group_sched teacher_sched aud)
      (init_sched tl storage)
;;

let rec init_sched_lecture (list_pair_lecture : string list list) storage =
  match list_pair_lecture with
  | [] -> success
  | hd :: tl ->
    conde
      [ fresh
          (group1name
             group2name
             group3name
             group4name
             teachername
             subjname
             group1
             group2
             group3
             group4
             teacher_sched
             aud
             new_storage)
          (init_sched_a_week teacher_sched)
          (init_sched_a_week group1)
          (init_sched_a_week group2)
          (init_sched_a_week group3)
          (init_sched_a_week group4)
          (init_sched_a_week aud)
          (Std.list ( !! ) hd
          === Std.list
                Fun.id
                [ group1name; group2name; group3name; group4name; teachername; subjname ]
          )
          (myassoco group1name storage group1)
          (myassoco group2name storage group2)
          (myassoco group3name storage group3)
          (myassoco group4name storage group4)
          (myassoco teachername storage teacher_sched)
          (insert_all_sched_lecture
             subjname
             group1
             group2
             group3
             group4
             teacher_sched
             aud)
          (* (insert_sched_in_storage group1name group1 storage)
          (insert_sched_in_storage group2name group2 storage)
          (insert_sched_in_storage group3name group3 storage)
          (insert_sched_in_storage group4name group4 storage)
          (insert_sched_in_storage teachername teacher_sched storage) *)
          (init_sched_lecture tl storage)
      ]
;;

let rec init_sched_new (list_pair : string list list) storage n =
  match n with
  | 0 -> success
  | n -> init_sched list_pair storage
;;

(* init_storage n storage
    &&& *)

let rec len q =
  match q with
  | [] -> 0
  | hd :: tl -> 1 + len tl
;;

let test1 : _ -> _ -> _ -> _ -> ianswer -> goal =
 fun _constaints schedule lecture_plan number answer ->
  fresh
    storage
    (init_storage number storage)
    (use_constraint storage _constaints)
    (init_sched schedule storage)
    (* (init_sched_lecture lecture_plan1 storage) *)
    (init_sched_lecture lecture_plan storage)
    (storage === answer)
;;
(* (len schedule * 10) *)
