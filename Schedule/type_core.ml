open OCanren
open OCanren.Std

module IAssoc = struct
  type key = int ilogic
  type 'a t = (key, 'a) Std.Pair.injected Std.List.injected

  let addo k v map map_new = Std.List.cons (Std.pair k v) map === map_new
end

let reifier ()
  : ( 'a ilogic Std.List.injected Std.List.injected Std.List.injected
  , 'a logic Std.List.logic Std.List.logic Std.List.logic ) Reifier.t
  =
  Std.List.reify (Std.List.reify (Std.List.reify OCanren.reify))
;;

type ischedule = int ilogic Std.List.injected Std.List.injected
type schedule_logic = int logic Std.List.logic Std.List.logic
type ianswer = (IAssoc.key, ischedule) Pair.groundi Std.List.injected
type normans = (IAssoc.key, ischedule) Pair.groundi Std.List.injected

let storage_reifier
  : (ianswer, (int logic, schedule_logic) Pair.logic Std.List.logic) Reifier.t
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
       (GT.show OCanren.logic @@ GT.show GT.int)
       (GT.show
          Std.List.logic
          (GT.show Std.List.logic (GT.show OCanren.logic @@ GT.show GT.int))))
;;

let show_storage1 =
  GT.show
    Std.List.list
    (GT.show
       Std.Pair.logic
       (GT.show OCanren.logic @@ GT.show GT.int)
       (GT.show
          Std.List.logic
          (GT.show Std.List.logic (GT.show OCanren.logic @@ GT.show GT.int))))
;;

let new_show_storage =
  GT.show
    Std.List.logic
    (GT.show
       Std.Pair.logic
       (GT.show OCanren.logic @@ GT.show GT.string)
       (GT.show
          Std.List.logic
          (GT.show Std.List.logic (GT.show OCanren.logic @@ GT.show GT.string))))
;;
