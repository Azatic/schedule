open OCanren
open OCanren.Std

let insert_first_lesson q =
  conde [ fresh (s w e r) (q === Std.list Fun.id [ s; w; e; r ]) (s === !!"werty") ]
;;
let rec membero x l = conde [ List.caro l x; fresh d (List.cdro l d) (membero x d) ]
let ins_first_lesson q lesson =
  conde [ fresh (s w e r) (q === Std.list Fun.id [ s; w; e; r ]) (s === lesson) ]
;;

let ins_second_lesson q lesson =
  conde [ fresh (s w e r) (q === Std.list Fun.id [ s; w; e; r ]) (w === lesson) ]
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

let insert_lesson q lesson =
  conde
    [ ins_monday_1 q lesson
    ; ins_monday_2 q lesson
    ; ins_monday_3 q lesson
    ; ins_monday_4 q lesson
    ; ins_tuesday_1 q lesson
    ; ins_tuesday_2 q lesson
    ; ins_tuesday_3 q lesson
    ; ins_tuesday_4 q lesson
    ]
;;

let init_sched_one_day s =
  conde [ fresh (q w e r) (s === Std.list Fun.id [ q; w; e; r ]) ]
;;

type ischedule = string ilogic Std.List.injected Std.List.injected
type schedule_logic = string logic Std.List.logic Std.List.logic

let init_sched_a_week : ischedule -> goal =
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

let insert_all_sched subj group_sched teacher_sched class_sched =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5)
    (group_sched === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (teacher_sched === Std.list Fun.id [ b1; b2; b3; b4; b5 ])
    (class_sched === Std.list Fun.id [ c1; c2; c3; c4; c5 ])
    (conde
       [ ins_sched1 subj a1 b1 c1
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
type week_day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

type group_or_teacher =
  | Teacher of { name : string }
  | Group of { number : int }

type user_constraints =
  | Group_not_learning of
      { number_lesson : int
      ; day : week_day
      }

type user_constraints_1 =
  | Not_learning of
      { number_lesson : int
      ; day : week_day
      ; group_or_teacher : group_or_teacher
      }

[@@@ocaml.warnerror "-27"]
let delete_monday_1 q =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 b6 b7 b8)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a1 === Std.list Fun.id [ !!"window"; b5; b6; b7 ])
    ]
;;

let delete_monday q =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a1 === Std.list Fun.id [ !!"window"; !!"window"; !!"window"; !!"window" ])
    ]
;;

let delete_tuesday q =
  conde
    [ fresh
        (a1 a2 a3 a4 a5 b1 b2 b3 b4)
        (q === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
        (a2 === Std.list Fun.id [ !!"window"; !!"window"; !!"window"; !!"window" ])
    ]
;;

let delete_day day q =
  match day with
  | Monday -> delete_monday q
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
    &&& conde [ a === key &&& (b === v); a =/= key &&& myassoco key tl v ; (xs === List.nil ()) &&& failure])
;;

module IAssoc = struct
  type key = string ilogic
  type 'a t = (key, 'a) Std.Pair.injected Std.List.injected

  let findo : key -> 'a ilogic t -> 'a ilogic -> goal = myassoco

  let addo k v map map_new =
    (* TODO: don't allow duplicate keys? *)
    Std.List.cons (Std.pair k v) map === map_new
  ;;
end

let reifier ()
  : ( 'a ilogic Std.List.injected Std.List.injected Std.List.injected
  , 'a logic Std.List.logic Std.List.logic Std.List.logic ) Reifier.t
  =
  Std.List.reify (Std.List.reify (Std.List.reify OCanren.reify))
;;

let storage_reifier
  : ( (IAssoc.key, ischedule) Pair.groundi Std.List.injected
  , (string logic, schedule_logic) Pair.logic Std.List.logic ) Reifier.t
  =
  Std.List.reify
    (Std.Pair.reify OCanren.reify (Std.List.reify (Std.List.reify OCanren.reify)))
;;

let shower =
  GT.show
    Std.List.logic
    (GT.show
       Std.List.logic
       (GT.show Std.List.logic (GT.show OCanren.logic @@ GT.show GT.string)))
;;

let show_storage =
  GT.show
    Std.List.logic
    (GT.show
       Std.Pair.logic
       (GT.show OCanren.logic @@ GT.show GT.string)
       (GT.show
          Std.List.logic
          (GT.show Std.List.logic (GT.show OCanren.logic @@ GT.show GT.string))))
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
let equal_log_str str = conde [ !!"matan" === !!str ]

let searcho _constaints answer =
  fresh
    (group1 group2 teacher1 t)
    (answer === Std.list Fun.id [ group1; group2; teacher1; t ])
    (relation_for_all_group_or_teacher _constaints group1 group2 teacher1)
    (init_sched_a_week group1)
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
  conde [fresh (a1 name) (init_sched_a_week a1) (q === Std.list Fun.id [Std.pair name a1])]

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
  | _ -> fresh (a b) (insert_storage (n-1) a) (ins_store b) (appendo a b q)
;;

let rec insert_sched_in_storage group_name group_sched storage =
  conde [fresh (a1 a2) (List.caro storage a1) (a1 === Std.pair group_name group_sched);
  fresh (a3) (List.cdro storage a3) (insert_sched_in_storage group_name group_sched a3)]
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
      (myassoco groupname (storage : ischedule IAssoc.t) group_sched)
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

(* let test1 _constaints answer =
  conde
    [ fresh
        (storage)
        (init_sched
          [ [ "b-10"; "teacher"; "matan" ]
           (* ; [ "b-10"; "teacher"; "matan2" ] *)
           (* ; [ "b-10"; "teacher"; "alg" ]
           ; [ "b-10"; "teacher"; "matan3" ]
           ; [ "b-10"; "teacher"; "matan4" ]
           ; [ "b-10"; "teacher"; "alg2" ] *)
           ]
           storage)
        (storage === answer)
    ]
;; *)

let schedo _constraints =
  (* [%tester run_r reifier shower 1 (* принимает расписание qtrs *) searcho]
     *)
  OCanren.run
    OCanren.q
    (fun x -> finish _constraints x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.iteri (fun i ans -> Format.printf "%d: %s\n%!" i (show_storage ans))
;;

(* let mytype () = Std.List.reify (Std.Pair.reify OCanren.reify (Std.List.reify OCanren.reify)) *)


let rec init_sched_new (list_pair : string list list) storage n =
  match n with
  | 0 -> success
  | n -> (insert_storage n storage) &&& 
  match list_pair with
  | [] -> success
  | hd::tl -> (init_sched list_pair storage) 
    
   
;;

let test1 _constaints schedule answer =
  conde
    [ fresh
        (storage )

        (init_sched_new 
          (* [ [ "b-10"; "teacher"; "matan" ]
            ; [ "b-10"; "teacher"; "matan2" ] 
            ; [ "b-10"; "teacher"; "alg" ]
           ; [ "b-10"; "teacher"; "matan3" ]
           ; [ "b-10"; "teacher"; "matan4" ]
           ; [ "b-10"; "teacher"; "alg2" ] 
           ; ["b-07" ; "viden" ; "matan"]
           ; ["b-08"; "teacher2"; "eng"]
           ; ["b-08" ; "teacher"; "eng10"]] *)
           schedule
           storage 10)
        (storage === answer)
    ]
;;

