(* stats.ml — Collect statistics about a CIL file *)
(* Counts semicolon-LOC by counting instructions, returns, and
   declarations — the CIL AST nodes that correspond to semicolons
   in the emitted C source. *)

open GoblintCil

(** Count semicolon-terminated statements in a CIL file.
    This counts: Set, Call, VarDecl instructions, Return stmts,
    and global variable definitions — matching the semicolons
    that appear in the emitted C output. *)
let count_semicolons (f : file) : int =
  let count = ref 0 in
  let vis = object
    inherit nopCilVisitor
    method! vinst (_i : instr) : instr list visitAction =
      incr count;
      SkipChildren
    method! vstmt s : stmt visitAction =
      (match s.skind with
       | Return _ -> incr count
       | _ -> ());
      DoChildren
  end in
  iterGlobals f (fun g ->
    match g with
    | GVar _ | GVarDecl _ -> incr count
    | GFun _ -> ignore (visitCilGlobal vis g)
    | _ -> ()
  );
  !count

type t = {
  semicolon_loc : int;
}

let collect (f : file) : t = {
  semicolon_loc = count_semicolons f;
}

let to_string (s : t) : string =
  Printf.sprintf "semicolon_loc=%d" s.semicolon_loc
