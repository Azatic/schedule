open OCanren
open OCanren.Std

let init_sched_one_day s =
  conde [ fresh (q w e r) (s === Std.list Fun.id [ q; w; e; r ]) ]
;;

let insert_first_session schedule subj = schedule === Std.list Fun.id [ subj; __; __; __ ]

let insert_second_session schedule subj =
  schedule === Std.list Fun.id [ __; subj; __; __ ]
;;

let insert_third_session schedule subj = schedule === Std.list Fun.id [ __; __; subj; __ ]

let insert_fourth_session schedule subj =
  schedule === Std.list Fun.id [ __; __; __; subj ]
;;

let init_sched_a_week q =
  conde
    [ fresh
        (w e r t y)
        (init_sched_one_day w)
        (init_sched_one_day e)
        (init_sched_one_day r)
        (init_sched_one_day t)
        (init_sched_one_day y)
        (q === Std.list Fun.id [ w; e; r; t; y ])
    ]
;;

let insert_lesson_to_first_session subj group_sched teacher_sched class_sched =
  fresh
    a2
    (insert_first_session group_sched subj)
    (insert_first_session teacher_sched subj)
    (insert_first_session class_sched subj)
;;

let insert_lesson_to_second_session subj group_sched teacher_sched class_sched =
  fresh
    a2
    (insert_second_session group_sched subj)
    (insert_second_session teacher_sched subj)
    (insert_second_session class_sched subj)
;;

let insert_lesson_to_third_session subj group_sched teacher_sched class_sched =
  fresh
    a2
    (insert_third_session group_sched subj)
    (insert_third_session teacher_sched subj)
    (insert_third_session class_sched subj)
;;

let insert_lesson_to_fourth_session subj group_sched teacher_sched class_sched =
  fresh
    a2
    (insert_fourth_session group_sched subj)
    (insert_fourth_session teacher_sched subj)
    (insert_fourth_session class_sched subj)
;;

let init_store q =
  fresh (a1 name) (init_sched_a_week a1) (q === Std.list Fun.id [ Std.pair name a1 ])
;;

let rec appendo a b ab =
  conde
    [ a === nil () &&& (b === ab)
    ; fresh (h t ab') (a === h % t) (h % ab' === ab) (appendo t b ab')
    ]
;;

let rec init_storage n q =
  match n with
  | 0 -> success
  | 1 -> init_store q
  | n -> fresh (a b) (init_store b) (init_storage (n - 1) a) (appendo a b q)
;;

let ins_lecture1 subj group1 group2 group3 group4 teacher_sched class_sched =
  fresh
    a2
    (insert_first_session group1 subj)
    (insert_first_session group2 subj)
    (insert_first_session group3 subj)
    (insert_first_session group4 subj)
    (insert_first_session teacher_sched subj)
    (insert_first_session class_sched subj)
;;

let ins_lecture2 subj group1 group2 group3 group4 teacher_sched class_sched =
  fresh
    a2
    (insert_second_session group1 subj)
    (insert_second_session group2 subj)
    (insert_second_session group3 subj)
    (insert_second_session group4 subj)
    (insert_second_session teacher_sched subj)
    (insert_second_session class_sched subj)
;;

let ins_lecture3 subj group1 group2 group3 group4 teacher_sched class_sched =
  fresh
    a2
    (insert_third_session group1 subj)
    (insert_third_session group2 subj)
    (insert_third_session group3 subj)
    (insert_third_session group4 subj)
    (insert_third_session teacher_sched subj)
    (insert_third_session class_sched subj)
;;

let ins_lecture4 subj group1 group2 group3 group4 teacher_sched class_sched =
  fresh
    a2
    (insert_fourth_session group1 subj)
    (insert_fourth_session group2 subj)
    (insert_fourth_session group3 subj)
    (insert_fourth_session group4 subj)
    (insert_fourth_session teacher_sched subj)
    (insert_fourth_session class_sched subj)
;;

let insert_lesson subj group_sched teacher_sched class_sched =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5)
    (group_sched === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (teacher_sched === Std.list Fun.id [ b1; b2; b3; b4; b5 ])
    (class_sched === Std.list Fun.id [ c1; c2; c3; c4; c5 ])
    (conde
       [ insert_lesson_to_second_session subj a1 b1 c1
       ; insert_lesson_to_third_session subj a1 b1 c1
       ; insert_lesson_to_second_session subj a2 b2 c2
       ; insert_lesson_to_third_session subj a2 b2 c2
       ; insert_lesson_to_second_session subj a3 b3 c3
       ; insert_lesson_to_third_session subj a3 b3 c3
       ; insert_lesson_to_second_session subj a4 b4 c4
       ; insert_lesson_to_third_session subj a4 b4 c4
       ; insert_lesson_to_second_session subj a5 b5 c5
       ; insert_lesson_to_third_session subj a5 b5 c5
       ; insert_lesson_to_fourth_session subj a5 b5 c5
       ; insert_lesson_to_first_session subj a1 b1 c1
       ; insert_lesson_to_fourth_session subj a1 b1 c1
       ; insert_lesson_to_first_session subj a2 b2 c2
       ; insert_lesson_to_fourth_session subj a2 b2 c2
       ; insert_lesson_to_first_session subj a3 b3 c3
       ; insert_lesson_to_fourth_session subj a3 b3 c3
       ; insert_lesson_to_first_session subj a4 b4 c4
       ; insert_lesson_to_fourth_session subj a4 b4 c4
       ; insert_lesson_to_first_session subj a5 b5 c5
       ])
;;

let insert_lecture subj group1 group2 group3 group4 teacher_sched class_sched =
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
       [ ins_lecture3 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture4 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture3 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture4 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture3 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture4 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture3 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture4 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture3 subj a5 d5 e5 f5 b5 c5
       ; ins_lecture4 subj a5 d5 e5 f5 b5 c5
       ; ins_lecture1 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture2 subj a1 d1 e1 f1 b1 c1
       ; ins_lecture1 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture2 subj a2 d2 e2 f2 b2 c2
       ; ins_lecture1 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture2 subj a3 d3 e3 f3 b3 c3
       ; ins_lecture1 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture2 subj a4 d4 e4 f4 b4 c4
       ; ins_lecture1 subj a5 d5 e5 f5 b5 c5
       ; ins_lecture2 subj a5 d5 e5 f5 b5 c5
       ])
;;
