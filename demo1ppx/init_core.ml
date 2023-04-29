open OCanren
open OCanren.Std

let init_sched_one_day s =
  conde [ fresh (q w e r) (s === Std.list Fun.id [ q; w; e; r ]) ]
;;

let init_sched_a_week : Type_core.ischedule -> goal =
 fun q ->
  let ( ** ) = ( % ) in
  conde
    [ fresh
        (w e r t y)
        (init_sched_one_day w)
        (init_sched_one_day e)
        (init_sched_one_day r)
        (init_sched_one_day t)
        (init_sched_one_day y)
        (q === w ** e ** r ** t ** !<y)
    ]
;;

let ins_monday_1 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a1 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b1 === lesson)
    ]
;;

let ins_monday_2 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a1 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b2 === lesson)
    ]
;;

let ins_monday_3 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a1 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b3 === lesson)
    ]
;;

let ins_monday_4 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a1 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b4 === lesson)
    ]
;;

let ins_tuesday_1 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a2 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b1 === lesson)
    ]
;;

let ins_tuesday_2 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a2 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b2 === lesson)
    ]
;;

let ins_tuesday_3 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a2 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b3 === lesson)
    ]
;;

let ins_tuesday_4 q lesson =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a2 === Std.list Fun.id [ b1; b2; b3; b4 ])
        (b4 === lesson)
    ]
;;

let ins_sched1 subj group_sched teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4)
        (group_sched === Std.list Fun.id [ subj; a2; a3; a4 ])
        (teacher_sched === Std.list Fun.id [ subj; b2; b3; b4 ])
        (class_sched === Std.list Fun.id [ subj; c2; c3; c4 ])
    ]
;;

let ins_sched2 subj group_sched teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4)
        (group_sched === Std.list Fun.id [ a2; subj; a3; a4 ])
        (teacher_sched === Std.list Fun.id [ b2; subj; b3; b4 ])
        (class_sched === Std.list Fun.id [ c2; subj; c3; c4 ])
    ]
;;

let ins_sched3 subj group_sched teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4)
        (group_sched === Std.list Fun.id [ a2; a3; subj; a4 ])
        (teacher_sched === Std.list Fun.id [ b2; b3; subj; b4 ])
        (class_sched === Std.list Fun.id [ c2; c3; subj; c4 ])
    ]
;;

let ins_sched4 subj group_sched teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4)
        (group_sched === Std.list Fun.id [ a2; a3; a4; subj ])
        (teacher_sched === Std.list Fun.id [ b2; b3; b4; subj ])
        (class_sched === Std.list Fun.id [ c2; c3; c4; subj ])
    ]
;;
