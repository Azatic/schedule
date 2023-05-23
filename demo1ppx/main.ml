open Sched_core
open Js_of_ocaml
open Type_core

type key_desc = string

open OCanren

let schedo _constraints schedule lecture_plan =
  let open OCanren in
  OCanren.run
    OCanren.q
    (fun x -> test1 _constraints schedule lecture_plan x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.map (fun storage ->
       print_endline (show_storage storage);
       let storage : (key_desc * string list list) list =
         storage
         |> OCanren.Std.List.logic_to_ground_exn (function
              | OCanren.Value (a, b) ->
                ( OCanren.from_logic a
                , OCanren.Std.List.logic_to_ground_exn
                    (OCanren.Std.List.logic_to_ground_exn
                       Fun.(
                         fun x ->
                           match x with
                           | Value x -> x
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
       |> Js.array)
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
           (to_list
              (map
                 to_list
                 (lecture_plan
                 |> Js.to_array
                 |> Array.map Js.to_array
                 |> Array.map (Array.map Js.to_string))))
    end)
;;
