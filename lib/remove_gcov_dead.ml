(* remove_gcov_dead.ml — Remove code proven dead by gcov runtime profiling
 *
 * Uses gcov coverage data to remove:
 *   1. Functions that were never called (execution_count = 0)
 *   2. Statements/instructions on lines that were never executed (count = 0)
 *
 * Safety:
 *   - Only removes code gcov proves was never reached at runtime
 *   - For deterministic Csmith programs (single input), one run captures
 *     all reachable code — so this is sound
 *   - Never removes main()
 *   - Preserves labeled statements (goto targets)
 *   - Preserves statements whose lines gcov didn't instrument
 *
 * This transform should run EARLY in the pipeline (after constant folding)
 * because removing entire dead functions maximizes downstream cleanup.
 *
 * The gcov data is loaded by slicer.ml and stored in [gcov_data_ref].
 *)

open GoblintCil

(** Global ref holding the gcov coverage data.
    Set by slicer.ml before the pipeline runs.
    If None, this transform is a no-op. *)
let gcov_data_ref : Gcov_data.t option ref = ref None

(* ── Does this statement carry any labels? ── *)
let has_labels (s : stmt) : bool =
  s.labels <> []

(* ── Does this statement or its children contain labels? ── *)
let rec stmt_has_labels_deep (s : stmt) : bool =
  if has_labels s then true
  else match s.skind with
  | Block b | Loop (b, _, _, _, _) -> block_has_labels_deep b
  | If (_, tb, fb, _, _) -> block_has_labels_deep tb || block_has_labels_deep fb
  | Switch (_, b, _, _, _) -> block_has_labels_deep b
  | _ -> false

and block_has_labels_deep (b : block) : bool =
  List.exists stmt_has_labels_deep b.bstmts

(* ── Get the line number from a location ── *)
let line_of_loc (loc : location) : int =
  loc.line

(* ── Get the location of an instruction ── *)
let loc_of_instr (i : instr) : location =
  match i with
  | Set (_, _, _, loc) -> loc
  | Call (_, _, _, _, loc) -> loc
  | VarDecl (_, loc) -> loc
  | Asm (_, _, _, _, _, loc) -> loc

(* ── Get the location of a statement ── *)
let loc_of_stmt (s : stmt) : location =
  match s.skind with
  | Instr (i :: _) -> loc_of_instr i
  | Return (_, loc, _) -> loc
  | Goto (_, loc) -> loc
  | ComputedGoto (_, loc) -> loc
  | Break loc -> loc
  | Continue loc -> loc
  | If (_, _, _, loc, _) -> loc
  | Switch (_, _, _, loc, _) -> loc
  | Loop (_, loc, _, _, _) -> loc
  | Block _ -> locUnknown
  | Instr [] -> locUnknown

(* ── Phase 1: Collect references to all functions ──
   Build a set of function names that are referenced (called or address-taken)
   anywhere in the file.  We only remove functions that are BOTH gcov-dead
   AND not referenced from any remaining code. *)

let collect_referenced_functions (f : file) : (string, bool) Hashtbl.t =
  let refs = Hashtbl.create 64 in
  let vis = object
    inherit nopCilVisitor
    method! vexpr (e : exp) : exp visitAction =
      (match e with
       | Lval (Var vi, NoOffset) when isFunctionType vi.vtype ->
         Hashtbl.replace refs vi.vname true
       | AddrOf (Var vi, NoOffset) when isFunctionType vi.vtype ->
         Hashtbl.replace refs vi.vname true
       | _ -> ());
      DoChildren
    method! vinst (i : instr) : instr list visitAction =
      (match i with
       | Call (_, Lval (Var vi, NoOffset), _, _, _) ->
         Hashtbl.replace refs vi.vname true
       | _ -> ());
      DoChildren
  end in
  visitCilFileSameGlobals vis f;
  refs

(* ── Phase 1b: Remove dead functions ──
   Only removes functions that are:
   - gcov-proven never executed (execution_count = 0)
   - NOT main
   - NOT referenced from any other code in the file *)
class removeDeadFunctionsVisitor (data : Gcov_data.t) (refs : (string, bool) Hashtbl.t) (removed_fns : int ref) = object
  inherit nopCilVisitor

  method! vglob (g : global) : global list visitAction =
    match g with
    | GFun (fd, _) ->
      let name = fd.svar.vname in
      if name <> "main"
         && Gcov_data.is_dead_function data name
         && not (Hashtbl.mem refs name) then begin
        Printf.eprintf "[remove_gcov_dead]   Removing dead function: %s (unreferenced + never executed)\n%!" name;
        incr removed_fns;
        ChangeTo []
      end else
        DoChildren
    | _ -> DoChildren
end

(* ── Phase 2: Remove dead statements/instructions ──
   Removes instructions and statements on lines gcov reports as dead.
   Conservative: preserves labeled stmts, uninstrumented lines, and
   statements whose removal could alter control flow. *)
class removeDeadStatementsVisitor (data : Gcov_data.t) (removed_stmts : int ref) = object
  inherit nopCilVisitor

  method! vinst (i : instr) : instr list visitAction =
    let loc = loc_of_instr i in
    let line = line_of_loc loc in
    if line > 0 && Gcov_data.is_safely_dead_line data line then begin
      (* Only remove assignments and var decls — keep calls for safety
         unless gcov also says the line is dead *)
      match i with
      | Set _ | VarDecl _ ->
        incr removed_stmts;
        ChangeTo []
      | Call _ ->
        (* Remove calls too if gcov says line was never reached *)
        incr removed_stmts;
        ChangeTo []
      | Asm _ ->
        (* Keep inline asm — too risky to remove *)
        SkipChildren
    end else
      SkipChildren

  method! vstmt (s : stmt) : stmt visitAction =
    (* Don't remove labeled statements — they may be goto targets *)
    if has_labels s then DoChildren
    else begin
      let loc = loc_of_stmt s in
      let line = line_of_loc loc in
      if line > 0 && Gcov_data.is_safely_dead_line data line
         && not (stmt_has_labels_deep s) then begin
        match s.skind with
        | Return _ ->
          (* Never remove returns — they affect control flow *)
          DoChildren
        | Goto _ | ComputedGoto _ | Break _ | Continue _ ->
          (* Don't remove control flow jumps *)
          DoChildren
        | If _ | Switch _ | Loop _ ->
          (* For compound statements, recurse into children —
             gcov may report the header line as dead but inner parts
             might have labels. Let vinst handle the leaf instructions. *)
          DoChildren
        | Block _ ->
          (* Recurse into blocks *)
          DoChildren
        | Instr _ ->
          (* Leaf instruction statements — vinst handles these *)
          DoChildren
      end else
        DoChildren
    end
end

(* ── Entry point ── *)
let apply (f : file) : unit =
  match !gcov_data_ref with
  | None ->
    Printf.eprintf "[remove_gcov_dead] No gcov data — skipping\n%!"
  | Some data ->
    (* Phase 1: Remove dead functions (only if unreferenced too) *)
    let refs = collect_referenced_functions f in
    let removed_fns = ref 0 in
    visitCilFile (new removeDeadFunctionsVisitor data refs removed_fns) f;
    Printf.eprintf "[remove_gcov_dead] Removed %d dead functions\n%!" !removed_fns;

    (* Phase 2: Remove dead statements/instructions *)
    let removed_stmts = ref 0 in
    visitCilFileSameGlobals (new removeDeadStatementsVisitor data removed_stmts) f;
    Printf.eprintf "[remove_gcov_dead] Removed %d dead instructions/statements\n%!" !removed_stmts

let transform : Transform.t = {
  name = "Remove gcov-proven dead code";
  apply;
}
