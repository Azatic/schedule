open Sched_core
open OCanren
open Type_core

let func schedule lecture_plan storage =
  (* print_endline (show_storage storage); *)
  let storage =
    storage
    |> OCanren.Std.List.logic_to_ground_exn (function
         | OCanren.Value (a, b) ->
           ( List.assoc
               (OCanren.from_logic a)
               (anti_list_str_to_int
                  (remove_duplicates (list_of_group_and_teacher schedule lecture_plan)))
           , OCanren.Std.List.logic_to_ground_exn
               (OCanren.Std.List.logic_to_ground_exn
                  Fun.(
                    fun x ->
                      match x with
                      | Value x ->
                        List.assoc
                          x
                          (Stdlib.List.append
                             (anti_list_str_to_int
                                (remove_duplicates (list_of_lesson schedule lecture_plan)))
                             [ -1, "adf" ])
                      | Var _ -> ""))
               b )
         | Var _ -> failwith "should not happend. (DONT DO THIS)")
  in
  storage
;;

let schedo constraints schedule lecture_plan no_formal_constr =
  OCanren.run
    OCanren.q
    (fun x ->
      Sched_core.generate_schedule constraints schedule lecture_plan no_formal_constr x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> List.map (func schedule lecture_plan)
  |> Stdlib.List.iter (fun ans ->
       List.iter
         (fun (x, y) ->
           print_endline x;
           (* List.iter (fun x -> List.iter (fun z -> Format.printf "%s" z) x) y) *)
           List.iter (fun x -> print_endline @@ String.concat " " x) y)
         ans)
;;

(* let t = Sys.time () *)

let _ =
  (* time *)
  schedo
    [ [ "2021pi-1"; "tuesday"; "4" ]; [ "2021pi-1"; "friday"; "5" ] ]
    [ [ "2021pi-1"; "Solev"; "teorver1" ]
    ; [ "2021pi-2"; "Solev"; "teorver2" ]
    ; [ "2021pi-1"; "Basov"; "diff1" ]
    ; [ "2021pi-2"; "Basov"; "diff2" ]
      (* ; [ "2021pi-1"; "Starchak"; "matlog1" ]
    ; [ "2021pi-2"; "Starchak"; "matlog2" ]
    ; [ "2021pi-1"; "Sartasov"; "practice1" ]
    ; [ "2021pi-2"; "Sartasov"; "practice2" ]
    ; [ "2022pi-1"; "Ivanova"; "algebra1" ]
    ; [ "2022pi-2"; "Ivanova"; "algebra2" ]
    ; [ "2022pi-1"; "Dodonov"; "matan1" ]
    ; [ "2022pi-2"; "Dodonov"; "matan2" ]
    ; [ "2022pi-1"; "Kalnitckiy"; "geom1" ]
    ; [ "2022pi-2"; "Kalnitckiy"; "geom2" ] *)
      (* ; [ "2020pi-2"; "Grigoriev"; "Graph_theory1" ] *)
      (* ; [ "2020pi-1"; "Grigoriev"; "Graph_theory2" ] *)
    ]
    [ [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Sartasov"; "Rpo" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Basov"; "Diff1" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Starchak"; "Matlog1" ]
      (* ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Burova"; "Math" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Luciv"; "Arkhitektura" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Luciv"; "Algorithm" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Kirilenko"; "Programming_base" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Mokaev"; "Math_disk" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Sivatckiy"; "Algebra" ]
    ; [ "2022pi-1"; "2022pi-2"; "zagl3"; "zagl4"; "Kalnitckiy"; "Geom" ]
    ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6"; "Zelenchuk"; "Telik" ]
    ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6"; "Zelenchuk"; "Telik1" ] *)
      (* ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6";  "Litvinov";"Prog" ] *)
      (* ; [ "2020pi-1"; "2020pi-2"; "zagl5"; "zagl6";"Grigoriev"; "Graph"  ] *)
    ]
    [ [ "2021pi-2"; "2021pi-1"; "Basov"; "diff2" ] ]
;;
