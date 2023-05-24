open Sched_core
open Js_of_ocaml
open Type_core
open OCanren

type key_desc = string

let func schedule lecture_plan storage =
  print_endline (show_storage storage);
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
                             [ -1, "окно" ])
                      | Var _ -> ""))
               b )
         | Var _ -> failwith "should not happend. (DONT DO THIS)")
  in
  storage
  |> Array.of_list
  |> Array.map (fun (a, b) ->
       object%js
         val x = Js.string a

         val schedule =
           Js.array (Array.of_list b |> Array.map Array.of_list)
           |> Js.array_map Js.array
           |> Js.array_map (Js.array_map Js.string)
       end)
  |> Js.array
;;

let schedo1 constraints schedule lecture_plan =
  OCanren.run
    OCanren.q
    (fun x -> Sched_core.test1 constraints schedule lecture_plan x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> List.map (func schedule lecture_plan)
  |> Array.of_list
  |> Js.array
;;

open Js_of_ocaml

[@@@ocaml.warnerror "-27"]

open Array

let _ =
  Js.export
    "myMathLib"
    (object%js
       method generateSched
         constraints
         (schedule : Js.js_string Js.t Js.js_array Js.t Js.js_array Js.t)
         lecture_plan =
         schedo1
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
           (to_list
              (map
                 to_list
                 (lecture_plan
                 |> Js.to_array
                 |> Array.map Js.to_array
                 |> Array.map (Array.map Js.to_string))))
    end)
;;
