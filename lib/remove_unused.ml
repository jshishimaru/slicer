(* remove_unused.ml — Remove unreachable globals *)
(* Wraps CIL's RmUnused.removeUnused to strip global variables,
   types, and functions not transitively reachable from main().
   Uses isCompleteProgramRoot so that only main and constructor/
   destructor functions are treated as roots; every other global
   that is not transitively reachable is removed. *)

open GoblintCil

let apply (f : file) : unit =
  RmUnused.removeUnused ~isRoot:RmUnused.isCompleteProgramRoot f

let transform : Transform.t = {
  name = "Remove unused globals";
  apply;
}
