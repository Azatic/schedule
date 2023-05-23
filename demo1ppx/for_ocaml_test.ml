(* open Lib *)

(* open Sched_core *)
open OCanren
open Type_core

let schedo constraints schedule lecture_plan =
  OCanren.run
    OCanren.q
    (fun x -> Sched_core.test1 constraints schedule lecture_plan x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.iteri (fun i ans -> Format.printf "%d: %s\n%!" i (show_storage ans))
;;

let t = Sys.time ()

let _ =
  (* time *)
  schedo
    (* [ [ "2021pi-1"; "tuesday"; "4" ] ] *)
    []
    [ [ "2021pi-1"; "Solev"; "teorver1" ]
    ; [ "2021pi-2"; "Solev"; "teorver2" ]
    ; [ "2021pi-1"; "Basov"; "diff1" ]
    ; [ "2021pi-2"; "Basov"; "diff2" ]
    ; [ "2021pi-1"; "Starchak"; "matlog1" ]
    ; [ "2021pi-2"; "Starchak"; "matlog2" ]
    ; [ "2021pi-1"; "Sartasov"; "practice1" ]
    ; [ "2021pi-2"; "Sartasov"; "practice2" ]
    ; [ "2022pi-1"; "Ivanova"; "algebra1" ]
    ; [ "2022pi-2"; "Ivanova"; "algebra2" ]
    ; [ "2022pi-1"; "Dodonov"; "matan1" ]
    ; [ "2022pi-2"; "Dodonov"; "matan2" ]
    ; [ "2022pi-1"; "Kalnitckiy"; "geom1" ]
    ; [ "2022pi-2"; "Kalnitckiy"; "geom2" ]
    ; [ "2020pi-2"; "Grigoriev"; "Graph_theory1" ]
    ; [ "2020pi-1"; "Grigoriev"; "Graph_theory2" ]
    ]
    [ [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Sartasov"; "Rpo" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Basov"; "Diff1" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Starchak"; "Matlog1" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Burova"; "Math" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Luciv"; "Arkhitektura" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Luciv"; "Algorithm" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Kirilenko"; "Programming_base" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Mokaev"; "Math_disk" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Sivatckiy"; "Algebra" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Kalnitckiy"; "Geom" ]
    ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6"; "Telik"; "Zelenchuk" ]
    ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6"; "Telik"; "Zelenchuk" ]
    ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6"; "Prog"; "Litvinov" ]
    ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6"; "Graph"; "Grigoriev" ]
    ]
;;

(* без разделения работало за 105 секунд *)
Printf.printf "Execution time: %fs\n%!" (Sys.time () -. t)

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
