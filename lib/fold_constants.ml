(* fold_constants.ml — Constant expression folding *)
(* Uses CIL's built-in constFoldVisitor to replace compile-time
   constant expressions with their computed values.
   Example: (1 << 4) - 1  →  15 *)

open GoblintCil

let apply (f : file) : unit =
  visitCilFileSameGlobals (constFoldVisitor true) f

let transform : Transform.t = {
  name = "Constant folding";
  apply;
}
