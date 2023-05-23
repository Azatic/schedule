open List

let list_of_group_or_teacher schedule lecture_plan =
  concat
    (append
       (map
          (fun x : string list ->
            match x with
            | hd1 :: hd2 :: tl -> [ hd1; hd2 ]
            | _ -> [])
          schedule
         : string list list)
       (map
          (fun x : string list ->
            match x with
            | hd1 :: hd2 :: hd3 :: hd4 :: hd5 :: tl -> [ hd1; hd2; hd3; hd4; hd5 ]
            | _ -> [])
          lecture_plan
         : string list list))
;;

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

let insert_sched_to_session session = conde [ fresh (group subj teacher) success ]

let rec insert_sched_in_storage group_name group_sched storage =
  fresh a1 (myassoco group_name storage a1) (a1 === group_sched)
;;

let rec init_sched (list_session : string list list) storage =
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
      (myassoco teachername storage (teacher_sched : ischedule))
      (Init_core.insert_lesson subjname group_sched teacher_sched aud)
      (debug_var storage (Fun.flip Type_core.storage_reifier) (function
        | [ s ] ->
          Printf.printf "%s\n%!" (String.concat " " hd);
          success
        | _ -> failwith "should not happen"))
      (init_sched tl storage)
;;

let rec init_sched_lecture (list_session_lecture : string list list) storage =
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
      (debug_var storage (Fun.flip Type_core.storage_reifier) (function
        | [ s ] ->
          Printf.printf "%s\n%!" (String.concat " " hd);
          success
        | _ -> failwith "should not happen"))
      (init_sched_lecture tl storage)
;;

let rec init_sched_new (list_session : string list list) storage n =
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

let rec append a b =
  match a with
  | [] -> b
  | h :: t -> h :: append t b
;;

let list_group_and_teacher schedule itog =
  let open List in
  match schedule with
  | hd1 :: hd2 :: tl -> append (append itog hd1) hd2
  | _ -> append itog []
;;

let list_group_and_teacher_lec lecture_plan itog =
  let open List in
  match lecture_plan with
  | hd1 :: hd2 :: hd3 :: hd4 :: hd5 :: tl ->
    append (append (append (append (append itog hd1) hd2) hd3) hd4) hd5
  | _ -> append itog []
;;

let rec map f l =
  match l with
  | [] -> []
  | h :: t -> f h :: map f t
;;

let test1 : _ -> _ -> _ -> ianswer -> goal =
 fun _constaints (schedule : string list list) lecture_plan answer ->
  fresh
    storage
    (init_storage
       (len (remove_duplicates (list_of_group_or_teacher schedule lecture_plan)))
       answer)
    (use_constraint answer _constaints)
    (init_sched_lecture lecture_plan answer)
    (init_sched schedule answer)
;;
