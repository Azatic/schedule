open OCanren
open OCanren.Std
open Constraint_core
open Init_core
open Type_core

let rec membero x l = conde [ List.caro l x; fresh d (List.cdro l d) (membero x d) ]

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

let rec init_sched (list_session : int list list) storage =
  match list_session with
  | [] -> success
  | hd :: tl ->
    fresh
      (groupname teachername subjname group_sched teacher_sched aud new_storage)
      (init_sched_a_week teacher_sched)
      (init_sched_a_week group_sched)
      (init_sched_a_week aud)
      (Std.list ( !! ) hd === Std.list Fun.id [ groupname; teachername; subjname ])
      (myassoco groupname storage group_sched)
      (myassoco teachername storage teacher_sched)
      (Init_core.insert_lesson subjname group_sched teacher_sched aud)
      (* (debug_var storage (Fun.flip Type_core.storage_reifier) (function
        | [ s ] ->
          Printf.printf "%s\n%!" (String.Stdlib.List.concat " " hd);
          success
        | _ -> failwith "should not happen")) *)
      (init_sched tl storage)
;;

let rec init_sched_lecture list_session_lecture storage =
  match list_session_lecture with
  | [] -> success
  | hd :: tl ->
    fresh
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
            [ group1name; group2name; group3name; group4name; teachername; subjname ])
      (myassoco group1name storage group1)
      (myassoco group2name storage group2)
      (myassoco group3name storage group3)
      (myassoco group4name storage group4)
      (myassoco teachername storage teacher_sched)
      (Init_core.insert_lecture subjname group1 group2 group3 group4 teacher_sched aud)
      (* (debug_var storage (Fun.flip Type_core.storage_reifier) (function
        | [ s ] ->
          Printf.printf "%s\n%!" (String.Stdlib.List.concat " " hd);
          success
        | _ -> failwith "should not happen")) *)
      (init_sched_lecture tl storage)
;;

let rec init_sched_new list_session storage n =
  match n with
  | 0 -> success
  | n -> init_sched list_session storage
;;

let rec len q =
  match q with
  | [] -> 0
  | hd :: tl -> 1 + len tl
;;

let rec remove_duplicates l =
  let rec contains l n =
    match l with
    | [] -> false
    | h :: t -> h = n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
    let acc = remove_duplicates t in
    if contains acc h then acc else h :: acc
;;

let nth l n =
  if n < 0
  then invalid_arg "List.nth"
  else (
    let rec nth_aux l n =
      match l with
      | [] -> failwith "nth"
      | a :: l -> if n = 0 then a else nth_aux l (n - 1)
    in
    nth_aux l n)
;;

let list_group_and_teacher schedule itog =
  let open List in
  remove_duplicates
    (match schedule with
     | hd1 :: hd2 :: tl -> itog :: hd1 :: hd2
     | _ -> itog :: [])
;;

let list_group_and_teacher_lec lecture_plan itog =
  let open List in
  remove_duplicates
    (match lecture_plan with
     | hd1 :: hd2 :: hd3 :: hd4 :: hd5 :: tl -> itog :: hd1 :: hd2 :: hd3 :: hd4 :: hd5
     | _ -> itog :: [])
;;

let list_of_group_and_teacher schedule lecture_plan =
  remove_duplicates
    (Stdlib.List.concat
       (Stdlib.List.append
          (Stdlib.List.map
             (fun x : string list ->
               match x with
               | hd1 :: hd2 :: tl -> [ hd1; hd2 ]
               | _ -> [])
             schedule
            : string list list)
          (Stdlib.List.map
             (fun x : string list ->
               match x with
               | hd1 :: hd2 :: hd3 :: hd4 :: hd5 :: tl -> [ hd1; hd2; hd3; hd4; hd5 ]
               | _ -> [])
             lecture_plan
            : string list list)))
;;

let list_of_lesson schedule lecture_plan =
  remove_duplicates
    (Stdlib.List.concat
       (Stdlib.List.append
          (Stdlib.List.map
             (fun x : string list ->
               match x with
               | hd1 :: hd2 :: tl -> tl
               | _ -> [])
             schedule
            : string list list)
          (Stdlib.List.map
             (fun x : string list ->
               match x with
               | hd1 :: hd2 :: hd3 :: hd4 :: hd5 :: tl -> tl
               | _ -> [])
             lecture_plan
            : string list list)))
;;

let list_str_to_dictionary sched = Stdlib.List.mapi (fun x i -> i, x) sched
let anti_list_str_to_int sched = Stdlib.List.mapi (fun x i -> x, i) sched

let list_list_string_to_int schedule_plan dict lesson_dict =
  Stdlib.List.map
    (fun x ->
      match x with
      | [ hd1; hd2; tl ] ->
        [ Stdlib.List.assoc hd1 dict
        ; Stdlib.List.assoc hd2 dict
        ; Stdlib.List.assoc tl lesson_dict
        ]
      | _ -> [])
    schedule_plan
;;

let list_list_string_to_int_for_constr constraints dict =
  let xs = [ "monday", 1; "tuesday", 2; "wednesday", 3; "thursday", 4; "friday", 5 ] in
  let xs2 = [ "1", 1; "2", 2; "3", 3; "4", 4; "5", 5 ] in
  Stdlib.List.map
    (fun x ->
      match x with
      | [ hd1; hd2; tl ] ->
        [ Stdlib.List.assoc hd1 dict; Stdlib.List.assoc hd2 xs; Stdlib.List.assoc tl xs2 ]
      | _ -> [])
    constraints
;;

let list_list_string_to_int_for_lecture schedule_plan dict lesson_dict =
  Stdlib.List.map
    (fun x ->
      match x with
      | [ hd1; hd2; hd3; hd4; hd5; tl ] ->
        [ Stdlib.List.assoc hd1 dict
        ; Stdlib.List.assoc hd2 dict
        ; Stdlib.List.assoc hd3 dict
        ; Stdlib.List.assoc hd4 dict
        ; Stdlib.List.assoc hd5 dict
        ; Stdlib.List.assoc tl lesson_dict
        ]
      | _ -> [])
    schedule_plan
;;

let test1 _constaints schedule lecture_plan answer =
  let dict_teacher_and_group =
    list_str_to_dictionary (list_of_group_and_teacher schedule lecture_plan)
  in
  let dict_lesson = list_str_to_dictionary (list_of_lesson schedule lecture_plan) in
  fresh
    storage
    (init_storage (len (list_of_group_and_teacher schedule lecture_plan)) storage)
    (use_constraint
       storage
       (list_list_string_to_int_for_constr _constaints dict_teacher_and_group))
    (init_sched
       (list_list_string_to_int schedule dict_teacher_and_group dict_lesson)
       storage)
    (init_sched_lecture
       (list_list_string_to_int_for_lecture
          lecture_plan
          dict_teacher_and_group
          dict_lesson)
       storage)
    (answer === storage)
;;
