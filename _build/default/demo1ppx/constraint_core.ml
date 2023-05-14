open OCanren
open OCanren.Std

let delete_monday q group =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4)
    (group === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (a1 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (conde
       [ q === !!"1" &&& (b1 === !!"window")
       ; q === !!"2" &&& (b2 === !!"window")
       ; q === !!"3" &&& (b3 === !!"window")
       ; q === !!"4" &&& (b4 === !!"window")
       ; q
         === !!"5"
         &&& (a1 === Std.list Fun.id [ !!"window"; !!"window"; !!"window"; !!"window" ])
       ])
;;

let delete_tuesday q group =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4)
    (group === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (a2 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (conde
       [ q === !!"1" &&& (b1 === !!"window")
       ; q === !!"2" &&& (b2 === !!"window")
       ; q === !!"3" &&& (b3 === !!"window")
       ; q === !!"4" &&& (b4 === !!"window")
       ; q
         === !!"5"
         &&& (a2 === Std.list Fun.id [ !!"window"; !!"window"; !!"window"; !!"window" ])
       ])
;;

let delete_wednesday q group =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4)
    (group === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (a3 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (conde
       [ q === !!"1" &&& (b1 === !!"window")
       ; q === !!"2" &&& (b2 === !!"window")
       ; q === !!"3" &&& (b3 === !!"window")
       ; q === !!"4" &&& (b4 === !!"window")
       ; q
         === !!"5"
         &&& (a3 === Std.list Fun.id [ !!"window"; !!"window"; !!"window"; !!"window" ])
       ])
;;

let delete_thursday q group =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4)
    (group === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (a4 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (conde
       [ q === !!"1" &&& (b1 === !!"window")
       ; q === !!"2" &&& (b2 === !!"window")
       ; q === !!"3" &&& (b3 === !!"window")
       ; q === !!"4" &&& (b4 === !!"window")
       ; q
         === !!"5"
         &&& (a4 === Std.list Fun.id [ !!"window"; !!"window"; !!"window"; !!"window" ])
       ])
;;

let delete_friday q group =
  fresh
    (a1 a2 a3 a4 a5 b1 b2 b3 b4)
    (group === Std.list Fun.id [ a1; a2; a3; a4; a5 ])
    (a5 === Std.list Fun.id [ b1; b2; b3; b4 ])
    (conde
       [ q === !!"1" &&& (b1 === !!"window")
       ; q === !!"2" &&& (b2 === !!"window")
       ; q === !!"3" &&& (b3 === !!"window")
       ; q === !!"4" &&& (b4 === !!"window")
       ; q
         === !!"5"
         &&& (a5 === Std.list Fun.id [ !!"window"; !!"window"; !!"window"; !!"window" ])
       ])
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

let delete_day n q group =
  conde
    [ n === !!"monday" &&& delete_monday q group
    ; n === !!"tuesday" &&& delete_tuesday q group
    ; n === !!"wednesday" &&& delete_wednesday q group
    ; n === !!"thursday" &&& delete_thursday q group
    ; n === !!"friday" &&& delete_friday q group
    ]
;;

let rec use_constraint storage list_pair_constraint =
  match list_pair_constraint with
  | [] -> success
  | hd :: tl ->
    conde
      [ fresh
          (group_name sched day number)
          (Std.list ( !! ) hd === Std.list Fun.id [ group_name; day; number ])
          (myassoco group_name storage sched)
          (delete_day day number sched)
      ]
    &&& use_constraint storage tl
;;
