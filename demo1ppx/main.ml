(* open Lib *)
open Sched_core

(* на данный момент мы передали некоторые ограничения, но пока никак их не используем *)
open Js_of_ocaml
open Type_core

let schedo _constraints schedule =
  (* [%tester run_r reifier shower 1 (* принимает расписание qtrs *) searcho]
   *)
  OCanren.run
    OCanren.q
    (fun x -> test1 _constraints schedule x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.iteri (fun i ans -> Format.printf "%d: %s\n%!" i (show_storage ans))
;;

open Array

let _ =
  Js.export
    "myMathLib"
    (object%js
       method insSched x y = x +. y
       method abs x = abs_float x

       method generateSched
         constraints
         (* (constraints : Js.js_string Js.t Js.js_array Js.t) *)
           (schedule : Js.js_string Js.t Js.js_array Js.t Js.js_array Js.t) =
         schedo
           (to_list
              (map
                 to_list
                 (constraints
                 |> Js.to_array
                 |> Array.map Js.to_array
                 |> Array.map (Array.map Js.to_string))))
           (to_list
              (map
                 to_list
                 (schedule
                 |> Js.to_array
                 |> Array.map Js.to_array
                 |> Array.map (Array.map Js.to_string))))

       val zero = 0.
    end)
;;
(* 
let _ =
  Js.export
    "sched"
    (object%js
       method ins_sched a b c d e = sched a b c d e
    end)
;; *)
