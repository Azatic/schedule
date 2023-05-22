open List

let sdf schedule lecture_plan =
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
open List

let rec membero x l = conde [ List.caro l x; fresh d (List.cdro l d) (membero x d) ]

(* let rec insert_sched_to_one_group
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
        (Init_core.insert_all_lecture lesson group_sched teacher_sched sched_class)
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
;; *)

(* let rec sched studyplanallgroup schedallgroup allteachersched schedclass classessubj =
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
;; *)

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
  conde
    [ fresh a1 (myassoco group_name storage a1) (a1 === group_sched)
    ; fresh (a1 a2) (List.caro storage a1) (a1 === Std.pair group_name group_sched)
    ; fresh a3 (List.cdro storage a3) (insert_sched_in_storage group_name group_sched a3)
    ]
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
      (init_sched tl storage)
;;

(* (debug_var storage (Fun.flip Type_core.storage_reifier) (function
        | [ s ] ->
          Printf.printf "%s\n%!" (String.concat " " hd);
          success
        | _ -> failwith "should not happen")) *)

let rec init_sched_lecture (list_session_lecture : string list list) storage =
  match list_session_lecture with
  | [] -> success
  | hd :: tl ->
    (* debug_var storage (Fun.flip Type_core.storage_reifier) (function
      | [ s ] ->
        Printf.printf "%s\n%!" (String.concat " " hd);
        success
      | _ -> failwith "should not happen")
    &&& *)
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
          (Init_core.insert_lecture
             subjname
             group1
             group2
             group3
             group4
             teacher_sched
             aud)
          (init_sched_lecture tl storage)
          (debug_var storage (Fun.flip Type_core.storage_reifier) (function
            | [ s ] ->
              Printf.printf "%s\n%!" (String.concat " " hd);
              success
            | _ -> failwith "should not happen"))
      ]
;;

let rec init_sched_new (list_session : string list list) storage n =
  match n with
  | 0 -> success
  | n -> init_sched list_session storage
;;

(* init_storage n storage
    &&& *)

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

(* val nth : 'a list -> int -> 'a
Return the n-th element of the given list. The first element (head of the list) is at position 0. *)
(* open List *)
(* 
let get_group_and_teacher_list schedule lecture_plan =
  (* let  mylist = ref[] in 
    for i =0 to (len schedule) do *)
  Std.List.concat schedule 
  
;; *)
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
       (* (len
          (remove_duplicates
             (list_group_and_teacher_lec
                lecture_plan
                (list_group_and_teacher schedule [])))) *)
       (* (len (list_group_and_teacher schedule [])) *)
       (len (remove_duplicates (sdf schedule lecture_plan)))
       answer)
    (use_constraint answer _constaints)
    (init_sched_lecture lecture_plan answer)
    (init_sched schedule answer)
;;

(* (storage === answer) *)

(* (len schedule * 10) *)

(* какие пока есть недостатки
   Constraint_core:
   нельзя передавать пустую строчку в html, а то все падает
    *)
open OCanren
open OCanren.Std

let rec appendo a b ab =
  conde
    [ a === nil () &&& (b === ab)
    ; fresh (h t ab') (a === h % t) (h % ab' === ab) (appendo t b ab')
    ]
;;

open Tester

let run_exn eta =
  run_r (Std.List.prj_exn OCanren.prj_exn) (GT.show Std.List.ground (GT.show GT.int)) eta
;;

let _ = run_exn (-1) qr qrh (REPR (fun q r -> appendo q r (list ( !! ) [ 1; 2; 3; 4 ])))
