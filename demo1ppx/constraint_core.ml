open OCanren
open OCanren.Std

let insert_window q first second third fourth day =
  match q with
  | 1 -> first === !!(-1)
  | 2 -> second === !!(-1)
  | 3 -> third === !!(-1)
  | 4 -> fourth === !!(-1)
  | 5 -> day === Std.list Fun.id [ !!(-1); !!(-1); !!(-1); !!(-1) ]
  | _ -> success
;;

let delete_monday q group =
  fresh
    (a1 b1 b2 b3 b4)
    (group === Std.list Fun.id [ a1; __; __; __; __ ])
    (a1 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a1)
;;

let delete_tuesday q group =
  fresh
    (a2 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; a2; __; __; __ ])
    (a2 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a2)
;;

let delete_wednesday q group =
  fresh
    (a3 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; __; a3; __; __ ])
    (a3 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a3)
;;

let delete_thursday q group =
  fresh
    (a4 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; __; __; a4; __ ])
    (a4 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a4)
;;

let delete_friday q group =
  fresh
    (a5 b1 b2 b3 b4)
    (group === Std.list Fun.id [ __; __; __; __; a5 ])
    (a5 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (insert_window q b1 b2 b3 b4 a5)
;;

let rec myassoco key xs v =
  conde
    [ xs === List.nil () &&& failure
    ; Fresh.three (fun a b tl ->
        xs
        === Std.pair a b % tl
        &&& conde [ a === key &&& (b === v); a =/= key &&& myassoco key tl v ])
    ]
;;

let delete_day n q group =
  match n with
  | 1 -> delete_monday q group
  | 2 -> delete_tuesday q group
  | 3 -> delete_wednesday q group
  | 4 -> delete_thursday q group
  | 5 -> delete_friday q group
  | _ -> success
;;

let rec use_constraint storage list_pair_constraint =
  match list_pair_constraint with
  | [] -> success
  | hd :: tl ->
    fresh
      (group_name sched)
      (match hd with
       | [ hd1; hd2; tl ] ->
         group_name
         === !!hd1
         &&& myassoco group_name storage sched
         &&& delete_day hd2 tl sched
       | _ -> success)
      (use_constraint storage tl)
;;
