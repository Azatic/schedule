open Sched_core

(* open Yojson.Basic *)
open Js_of_ocaml
open Type_core

type key_desc = string

open OCanren

(* let myfrom_logic = function
  | Value x -> x
  | Var (n, _) -> None
;; *)

let schedo _constraints schedule lecture_plan number =
  let open OCanren in
  OCanren.run
    OCanren.q
    (fun x -> test1 _constraints schedule lecture_plan number x)
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

              (* val schedule =
                   Js.array (Array.of_list [ "adsf"; "sdf" ]) |> Js.array_map Js.string *)

              val schedule =
                Js.array (Array.of_list b |> Array.map Array.of_list)
                |> Js.array_map Js.array
                |> Js.array_map (Js.array_map Js.string)
            end)
       |> Js.array)
  |> Array.of_list
  |> Js.array
;;

(* мне нужно в 33 34 строках вывести что-то содерджательное, т е само расписание *)
(* на данный момент мы передали некоторые ограничения, но пока никак их не используем *)
open Js_of_ocaml
(* open Type_core *)

(* open Yojson.Basic

(* Convert the data to JSON *)
let convert_to_json data =
let rec convert_list lst =
`List (List.map convert_item lst)

and convert_item item =
match item with
| (key, values) -> `Assoc [(key, convert_list values)]
| _ -> `Null

in
match data with
| (num, pairs) -> `Assoc [(string_of_int num, convert_list pairs)]

(* Data to convert *)
let data = (0, [
("b-11", [["geom1"; "geom2"; 0.475; 0.476]; [0.338; 0.339; 0.340; 0.341]; [0.346; 0.347; 0.348; 0.349]; [0.350; 0.351; 0.352; 0.353]; [0.362; 0.363; 0.364; 0.365]]);
("viden1", [["geom1"; "geom2"; 0.478; 0.479]; [0.272; 0.273; 0.274; 0.275]; [0.288; 0.289; 0.290; 0.291]; [0.292; 0.293; 0.294; 0.295]; [0.300; 0.301; 0.302; 0.303]])
])

(* Convert data to JSON *)
let json = convert_to_json data

(* Convert JSON to string *)
let json_string = to_string json

(* Print the JSON string *)
let () = print_endline json_string *)

(* let store_results = 
  let module J = Yojson.Basic in
  let module JU = Yojson.Basic.Util in
  let json = 
    match OCanren.Stream.take ~n:1 rr with
    | [] -> J.null
    | _::_ -> 
      let ans = rr#reify storage_reifier in
      let storage_json = ans |> show_storage |> J.from_string in
      J.(`Assoc ["storage", storage_json])
  in
  let file = open_out "results.json" in
  let _ = Yojson.Basic.pretty_to_channel file json in
  close_out file
in *)
[@@@ocaml.warnerror "-27"]

(* let schedo _constraints schedule lecture_plan =
  OCanren.run
    OCanren.q
    (fun x -> test1 _constraints schedule lecture_plan x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.iteri (fun i ans ->
       let json_obj = Yojson.Basic.from_string (show_storage ans) in
       let file_name = "output.json" in
       (* Set your desired file name and path *)
       Yojson.Basic.to_file file_name json_obj)
;;

let _ = schedo [] [ [ "b-07"; "viden"; "matan" ] ] [] *)
(* 
let schedo_to_json constraints schedule lecture_plan =
  OCanren.run
    OCanren.q
    (fun x -> Sched_core.test1 constraints schedule lecture_plan x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> List.map show_storage
  |> List.map Yojson.Safe.from_string
  |> `List
  |> Yojson.Safe.to_file "output.json"
;; *)

(* 

let schedo_to_json constraints schedule lecture_plan =
  OCanren.run
    OCanren.q
    (fun x -> Sched_core.test1 constraints schedule lecture_plan x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> List.map show_storage
  |> List.map Yojson.Safe.from_string
  |> `List
  |> Yojson.Safe.to_file "output.json"
(* Convert the data to JSON *)
let convert_to_json data =
  let rec convert_list lst = `List (List.map convert_item lst)
  and convert_item item =
    match item with
    | key, values -> `Assoc [ key, convert_list values ]
    | _ -> `Null
  in
  match data with
  | num, pairs -> `Assoc [ string_of_int num, convert_list pairs ]
;;

(* Data to convert *)
let data =
  ( 0
  , [ ( "b-11"
      , [ [ "geom1"; "geom2"; 0.475; 0.476 ]
        ; [ 0.338; 0.339; 0.340; 0.341 ]
        ; [ 0.346; 0.347; 0.348; 0.349 ]
        ; [ 0.350; 0.351; 0.352; 0.353 ]
        ; [ 0.362; 0.363; 0.364; 0.365 ]
        ] )
    ; ( "viden1"
      , [ [ "geom1"; "geom2"; 0.478; 0.479 ]
        ; [ 0.272; 0.273; 0.274; 0.275 ]
        ; [ 0.288; 0.289; 0.290; 0.291 ]
        ; [ 0.292; 0.293; 0.294; 0.295 ]
        ; [ 0.300; 0.301; 0.302; 0.303 ]
        ] )
    ] )
;;

(* Convert data to JSON *)
let json = convert_to_json data

(* Convert JSON to string *)
let json_string = to_string json

(* Print the JSON string *)
let () = print_endline json_string *)
(* let convert_to_json data =
  let rec convert_list lst = `List (List.map convert_item lst)
  and convert_item item =
    match item with
    | key, values -> `Assoc [ key, convert_list values ]
    | _ -> `Null
  in
  match data with
  | num, pairs -> `Assoc [ string_of_int num, convert_list pairs ]
;; *)
(* let schedo _constraints schedule lecture_plan =
  OCanren.run
    OCanren.q
    (fun x -> test1 _constraints schedule lecture_plan x)
    (fun rr -> rr#reify storage_reifier)
  |> OCanren.Stream.take ~n:1
  |> Stdlib.List.iteri (fun i data -> Format.printf "%d: %s\n%!" i (
  
  (* Data to convert *)

  
  (* Convert data to JSON *)
  let json = convert_to_json data
  
  (* Convert JSON to string *)
  let json_string = to_string json
  
  (* Print the JSON string *)
  let () = print_endline json_string))
;; *)

open Array

let _ =
  Js.export
    "myMathLib"
    (object%js
       method generateSched
         constraints
         (schedule : Js.js_string Js.t Js.js_array Js.t Js.js_array Js.t)
         lecture_plan
         number =
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
           number

       val zero = 0.
       method shit () = Js.array [| 1; 2; 3 |]
    end)
;;
