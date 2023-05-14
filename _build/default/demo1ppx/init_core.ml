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

let init_store q =
  conde
    [ fresh (a1 name) (init_sched_a_week a1) (q === Std.list Fun.id [ Std.pair name a1 ])
    ]
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
  | _ -> fresh (a b) (init_storage (n - 1) a) (init_store b) (appendo a b q)
;;

let ins_lecture1 subj group1 group2 group3 group4 teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4 d2 d3 d4 e2 e3 e4 f2 f3 f4)
        (group1 === Std.list Fun.id [ subj; a2; a3; a4 ])
        (group2 === Std.list Fun.id [ subj; d2; d3; d4 ])
        (group3 === Std.list Fun.id [ subj; e2; e3; e4 ])
        (group4 === Std.list Fun.id [ subj; f2; f3; f4 ])
        (teacher_sched === Std.list Fun.id [ subj; b2; b3; b4 ])
        (class_sched === Std.list Fun.id [ subj; c2; c3; c4 ])
    ]
;;

let ins_lecture2 subj group1 group2 group3 group4 teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4 d2 d3 d4 e2 e3 e4 f2 f3 f4)
        (group1 === Std.list Fun.id [ a2; subj; a3; a4 ])
        (group2 === Std.list Fun.id [ d2; subj; d3; d4 ])
        (group3 === Std.list Fun.id [ e2; subj; e3; e4 ])
        (group4 === Std.list Fun.id [ f2; subj; f3; f4 ])
        (teacher_sched === Std.list Fun.id [ b2; subj; b3; b4 ])
        (class_sched === Std.list Fun.id [ c2; subj; c3; c4 ])
    ]
;;

let ins_lecture3 subj group1 group2 group3 group4 teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4 d2 d3 d4 e2 e3 e4 f2 f3 f4)
        (group1 === Std.list Fun.id [ a2; a3; subj; a4 ])
        (group2 === Std.list Fun.id [ d2; d3; subj; d4 ])
        (group3 === Std.list Fun.id [ e2; e3; subj; e4 ])
        (group4 === Std.list Fun.id [ f2; f3; subj; f4 ])
        (teacher_sched === Std.list Fun.id [ b2; b3; subj; b4 ])
        (class_sched === Std.list Fun.id [ c2; c3; subj; c4 ])
    ]
;;

let ins_lecture4 subj group1 group2 group3 group4 teacher_sched class_sched =
  conde
    [ fresh
        (a2 a3 a4 b2 b3 b4 c2 c3 c4 d2 d3 d4 e2 e3 e4 f2 f3 f4)
        (group1 === Std.list Fun.id [ a2; a3; a4; subj ])
        (group2 === Std.list Fun.id [ d2; d3; d4; subj ])
        (group3 === Std.list Fun.id [ e2; e3; e4; subj ])
        (group4 === Std.list Fun.id [ f2; f3; f4; subj ])
        (teacher_sched === Std.list Fun.id [ b2; b3; b4; subj ])
        (class_sched === Std.list Fun.id [ c2; c3; c4; subj ])
    ]
;;
