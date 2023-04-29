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

let delete_day day q =
  match day with
  | Type_core.Monday -> Constraint_core.delete_monday q
  | Tuesday -> delete_tuesday q
  | _ -> failure
;;

let delete_day_with_number day number q =
  match day, number with
  | Monday, 5 -> delete_monday q
  | Tuesday, 5 -> delete_tuesday q
  | Monday, 1 -> delete_monday_1 q
  | _, _ -> failure
;;

let using_employ_user u_c q =
  match u_c with
  | "delete_monday" -> delete_monday q
  | "delete_tuesday" -> delete_tuesday q
  | _ -> failure
;;

let string_alg_type string =
  match string with
  (* match string with *)
  | "Delete_Monday_5_teacher1" ->
    Not_learning
      { number_lesson = 5
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher1" }
      }
  | "Delete_Monday_1_teacher1" ->
    Not_learning
      { number_lesson = 1
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher1" }
      }
  | "Delete_Tuesday_5_teacher1" ->
    Not_learning
      { number_lesson = 5
      ; day = Tuesday
      ; group_or_teacher = Teacher { name = "teacher1" }
      }
  | _ ->
    Not_learning
      { number_lesson = 1
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher2" }
      }
;;

let rec employ_constraint user_const q =
  match user_const with
  | [] -> success
  | hd :: tl -> using_employ_user hd q &&& employ_constraint tl q
;;

let use_user_constr alg_type q =
  match alg_type with
  | Not_learning { number_lesson; day; group_or_teacher } ->
    delete_day_with_number day number_lesson q
;;

let rec alg_constaint user_constraints q =
  match user_constraints with
  | [] -> success
  | hd :: tl -> use_user_constr (string_alg_type hd) q &&& alg_constaint tl q
;;

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

let string_alg_type string =
  match string with
  (* match string with *)
  | "Delete_Monday_5_teacher1" ->
    Not_learning
      { number_lesson = 5
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher1" }
      }
  | "Delete_Monday_1_teacher1" ->
    Not_learning
      { number_lesson = 1
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher1" }
      }
  | "Delete_Tuesday_5_teacher1" ->
    Not_learning
      { number_lesson = 5
      ; day = Tuesday
      ; group_or_teacher = Teacher { name = "teacher1" }
      }
  (* | [| "delete"; "Tuesday"; n ; "teacher"|] -> assert false  *)
  | _ ->
    Not_learning
      { number_lesson = 1
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher2" }
      }
;;

let ttt t1 t2 t3 = conde [ failure ]

let use_new_alg_const _const group1 group2 teacher1 =
  match _const with
  | Not_learning
      { number_lesson = 5
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher1" }
      } -> delete_day_with_number Monday 5 teacher1
  | Not_learning
      { number_lesson = 5
      ; day = Tuesday
      ; group_or_teacher = Teacher { name = "teacher1" }
      } -> delete_day_with_number Tuesday 5 teacher1
  | Not_learning
      { number_lesson = 1
      ; day = Monday
      ; group_or_teacher = Teacher { name = "teacher1" }
      } -> delete_day_with_number Monday 1 teacher1
  | Not_learning
      { number_lesson = 1; day = Monday; group_or_teacher = Group { number = 1 } } ->
    delete_day_with_number Monday 1 group1
  | Not_learning
      { number_lesson = 1; day = Monday; group_or_teacher = Group { number = 2 } } ->
    delete_day_with_number Monday 1 group2
  | Not_learning { number_lesson; day; group_or_teacher } -> failure
;;

let rec relation_for_all_group_or_teacher _constaints group1 group2 teacher1 =
  match _constaints with
  | [] -> success
  | hd :: tl ->
    use_new_alg_const (string_alg_type hd) group1 group2 teacher1
    &&& relation_for_all_group_or_teacher tl group1 group2 teacher1
;;

let searcho _constaints answer =
  fresh
    (group1 group2 teacher1 t)
    (answer === Std.list Fun.id [ group1; group2; teacher1; t ])
    (relation_for_all_group_or_teacher _constaints group1 group2 teacher1)
    (Init_core.init_sched_a_week group1)
    (init_sched_a_week t)
    (init_sched_a_week group2)
    (init_sched_a_week teacher1)
    (sched
       (Std.list
          Fun.id
          [ Std.list Fun.id [ !!"matan1"; !!"matan2"; !!"geom1"; !!"alg1" ]
          ; Std.list Fun.id [ !!"matan3"; !!"matan4"; !!"geom2"; !!"alg2" ]
          ])
       (Std.list Fun.id [ group1; group2 ])
       (Std.list
          Fun.id
          [ Std.list Fun.id [ teacher1; teacher1; teacher1; teacher1 ]
          ; Std.list Fun.id [ teacher1; teacher1; teacher1; teacher1 ]
          ])
       (Std.list Fun.id [ t ])
       (Std.list
          Fun.id
          [ Std.list
              Fun.id
              [ !!"matan1"
              ; !!"geom1"
              ; !!"eng1"
              ; !!"matan3"
              ; !!"matan4"
              ; !!"geom2"
              ; !!"alg2"
              ; !!"matan2"
              ; !!"matan6"
              ; !!"geom2"
              ; !!"alg1"
              ]
          ]))
;;

let insert_sched_to_pair pair = conde [ fresh (group subj teacher) success ]

let ins_store q =
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

let rec insert_storage n q =
  match n with
  | 0 -> success
  | 1 -> ins_store q
  | _ -> fresh (a b) (insert_storage (n - 1) a) (ins_store b) (appendo a b q)
;;

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
          (ins_store storage)
          (init_sched_a_week teacher_sched)
          (init_sched_a_week group_sched)
          (init_sched_a_week aud)
          (insert_all_sched subjname group_sched teacher_sched aud)
          (insert_sched_in_storage groupname group_sched storage)
          (insert_sched_in_storage teachername teacher_sched storage)
          (init_sched tl storage)
      ]
;;

let finish _constaints answer =
  conde
    [ fresh
        storage
        (init_sched
           [ [ "b-07"; "viden"; "geom1" ]
           ; [ "b-07"; "viden"; "alg1" ]
           ; [ "b-07"; "viden"; "matan1" ]
           ; [ "tyio"; "teacherww"; "matan" ]
           ]
           storage)
        (storage === answer)
    ]
;;

let schedo _constraints =
  OCanren.run
    OCanren.q
    (fun x -> finish _constraints x)
    (fun rr -> rr#reify Type_core.storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.iteri (fun i ans -> Format.printf "%d: %s\n%!" i (show_storage ans))
;;

let rec init_sched_new (list_pair : string list list) storage n =
  match n with
  | 0 -> success
  | n ->
    insert_storage n storage
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
    [ fresh storage (init_sched_new schedule storage (len schedule)) (storage === answer)
    ]
;;
