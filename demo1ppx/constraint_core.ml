open OCanren
open OCanren.Std

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
