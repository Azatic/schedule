open Sched_core
open OCanren
open Type_core

let func schedule lecture_plan storage =
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

[@@@ocaml.warnerror "-35"]

let _ =
  schedo
    [ [ "2021pi-1"; "tuesday"; "4" ]
    ; [ "2021pi-1"; "friday"; "5" ]
    ; [ "2021pi-1"; "thursday"; "5" ]
    ]
    [ [ "2021pi-1"; "Solev"; "teorver1" ]
    ; [ "2021pi-2"; "Solev"; "teorver2" ]
    ; [ "2021pi-1"; "Basov"; "diff1" ]
    ; [ "2021pi-2"; "Basov"; "diff2" ]
    ; [ "2021pi-1"; "Starchak"; "matlog1" ]
    ]
    [ [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Sartasov"; "Rpo" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Basov"; "Diff1" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Starchak"; "Matlog1" ]
    ; [ "2021pi-1"; "2021pi-2"; "zagl1"; "zagl2"; "Burova"; "Math" ]
    ]
    [ [ "2021pi-2"; "2021pi-1"; "Basov"; "diff2" ] ]
;;
