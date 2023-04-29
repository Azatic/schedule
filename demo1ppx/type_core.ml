open OCanren
open OCanren.Std

type ischedule = string ilogic Std.List.injected Std.List.injected
type schedule_logic = string logic Std.List.logic Std.List.logic

type week_day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

type group_or_teacher =
  | Teacher of { name : string }
  | Group of { number : int }

type user_constraints =
  | Group_not_learning of
      { number_lesson : int
      ; day : week_day
      }

type user_constraints_1 =
  | Not_learning of
      { number_lesson : int
      ; day : week_day
      ; group_or_teacher : group_or_teacher
      }

module IAssoc = struct
  type key = string ilogic
  type 'a t = (key, 'a) Std.Pair.injected Std.List.injected

  (* let findo : key -> 'a ilogic t -> 'a ilogic -> goal = myassoco *)
  let addo k v map map_new = Std.List.cons (Std.pair k v) map === map_new
end

let reifier ()
  : ( 'a ilogic Std.List.injected Std.List.injected Std.List.injected
  , 'a logic Std.List.logic Std.List.logic Std.List.logic ) Reifier.t
  =
  Std.List.reify (Std.List.reify (Std.List.reify OCanren.reify))
;;

let storage_reifier
  : ( (IAssoc.key, ischedule) Pair.groundi Std.List.injected
  , (string logic, schedule_logic) Pair.logic Std.List.logic ) Reifier.t
  =
  Std.List.reify
    (Std.Pair.reify OCanren.reify (Std.List.reify (Std.List.reify OCanren.reify)))
;;

let shower =
  GT.show
    Std.List.logic
    (GT.show
       Std.List.logic
       (GT.show Std.List.logic (GT.show OCanren.logic @@ GT.show GT.string)))
;;

let show_storage =
  GT.show
    Std.List.logic
    (GT.show
       Std.Pair.logic
       (GT.show OCanren.logic @@ GT.show GT.string)
       (GT.show
          Std.List.logic
          (GT.show Std.List.logic (GT.show OCanren.logic @@ GT.show GT.string))))
;;
