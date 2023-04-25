open Lib

let schedo _constraints schedule =
  (* [%tester run_r reifier shower 1 (* принимает расписание qtrs *) searcho]
   *)
  OCanren.run
    OCanren.q
    (fun x -> Sched_core.test1 _constraints schedule x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.iteri (fun i ans -> Format.printf "%d: %s\n%!" i (show_storage ans))
;;

(* |> fun () -> assert false *)

let _ = schedo [] [ [ "b-10"; "teacher"; "matan" ]
; [ "b-10"; "teacher"; "matan2" ] 
; [ "b-10"; "teacher"; "alg" ]
; [ "b-10"; "teacher"; "matan3" ]
; [ "b-10"; "teacher"; "matan4" ]
; [ "b-10"; "teacher"; "alg2" ] 
; ["b-07" ; "viden" ; "matan"]
; ["b-08"; "teacher2"; "eng"]
; ["b-08" ; "teacher"; "eng10"]] ;;
