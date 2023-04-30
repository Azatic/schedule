(* open OCanren
open OCanren.Std
open Constraint_core
open Type_core
open Init_core

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
    (Sched_core.sched
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
;; *)
