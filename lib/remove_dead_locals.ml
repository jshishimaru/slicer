(* remove_dead_locals.ml — Remove unused local variables and dead stores *)
(* Performs intra-procedural dead variable/store elimination.
   CIL's RmUnused only works at global scope; this handles locals.

   For each function:
   Phase 1 (Collect reads): Walk the body to find which local variable
     IDs appear in "read" positions (i.e., in expressions, not just as
     the LHS target of a Set).  Variables with vaddrof=true are
     conservatively treated as read.  Formals are never removed.
   Phase 2 (Eliminate dead): Remove Set/VarDecl instructions for dead
     locals, rewrite Call(Some dead_lv, ...) → Call(None, ...), and
     filter fd.slocals.

   Runs to a fixed-point: removing dead stores may cause previously-
   read variables (e.g. temporaries whose only consumer was a dead
   store) to become dead themselves.  Capped at 20 iterations. *)

open GoblintCil

(* ------------------------------------------------------------------ *)
(*  Phase 1 — collect the set of local variable IDs that are "read"   *)
(* ------------------------------------------------------------------ *)

(** Walk an expression tree and record every Var reference. *)
let rec collect_exp_reads (read_set : (int, bool) Hashtbl.t) (e : exp) : unit =
  match e with
  | Lval lv | StartOf lv | AddrOf lv ->
    collect_lval_reads read_set lv
  | UnOp (_, e1, _) -> collect_exp_reads read_set e1
  | BinOp (_, e1, e2, _) ->
    collect_exp_reads read_set e1;
    collect_exp_reads read_set e2
  | CastE (_, e1) -> collect_exp_reads read_set e1
  | Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOf _ | AlignOfE _
  | AddrOfLabel _ -> ()
  | Question (e1, e2, e3, _) ->
    collect_exp_reads read_set e1;
    collect_exp_reads read_set e2;
    collect_exp_reads read_set e3
  | Real e1 | Imag e1 -> collect_exp_reads read_set e1

and collect_lval_reads (read_set : (int, bool) Hashtbl.t) ((host, offset) : lval) : unit =
  (match host with
   | Var vi -> Hashtbl.replace read_set vi.vid true
   | Mem e  -> collect_exp_reads read_set e);
  collect_offset_reads read_set offset

and collect_offset_reads (read_set : (int, bool) Hashtbl.t) (off : offset) : unit =
  match off with
  | NoOffset -> ()
  | Field (_, o) -> collect_offset_reads read_set o
  | Index (e, o) ->
    collect_exp_reads read_set e;
    collect_offset_reads read_set o

(** Check if a type (or any nested element) is volatile. *)
let rec type_has_volatile (t : typ) : bool =
  let attrs = typeAttrs t in
  if hasAttribute "volatile" attrs then true
  else match t with
    | TArray (et, _, _) -> type_has_volatile et
    | _ -> false

(** Collect all read variable IDs in a function.
    Returns a Hashtbl mapping vid -> true for every local that is read. *)
let collect_reads (fd : fundec) : (int, bool) Hashtbl.t =
  let read_set = Hashtbl.create 64 in
  (* Formals are always considered read *)
  List.iter (fun vi -> Hashtbl.replace read_set vi.vid true) fd.sformals;
  (* Address-taken locals are always considered read *)
  List.iter (fun vi ->
    if vi.vaddrof then Hashtbl.replace read_set vi.vid true;
    (* Volatile locals: stores have side effects, treat as read *)
    if type_has_volatile vi.vtype then Hashtbl.replace read_set vi.vid true
  ) fd.slocals;

  (* Walk instructions and statements to find read positions *)
  let vis = object
    inherit nopCilVisitor

    method! vinst (i : instr) : instr list visitAction =
      (match i with
       | Set ((_host, _off) as lv, rhs, _, _) ->
         (* The RHS is a read position *)
         collect_exp_reads read_set rhs;
         (* The LHS offset sub-expressions are reads (e.g. arr[i] = ... reads i) *)
         (match lv with
          | (Var _vi, NoOffset) -> ()  (* simple var = ...; LHS var is NOT a read *)
          | (Var _vi, off) ->
            (* arr[expr] = ...; the index expression is a read,
               and _vi itself is read (it's being indexed into) *)
            Hashtbl.replace read_set _vi.vid true;
            collect_offset_reads read_set off
          | (Mem e, off) ->
            collect_exp_reads read_set e;
            collect_offset_reads read_set off)
       | Call (result_lv, fn_exp, args, _, _) ->
         (* The function expression and all args are reads *)
         collect_exp_reads read_set fn_exp;
         List.iter (collect_exp_reads read_set) args;
         (* If the result is stored into a complex lvalue, those sub-exprs are reads *)
         (match result_lv with
          | Some (Var _vi, NoOffset) -> ()  (* simple var = call(); LHS is NOT a read *)
          | Some lv -> collect_lval_reads read_set lv
          | None -> ())
       | VarDecl _ -> ()
       | Asm _ -> ()  (* inline asm: conservatively skip, formals/address-taken already handled *)
      );
      SkipChildren

    method! vstmt (s : stmt) : stmt visitAction =
      (match s.skind with
       | Return (Some e, _, _) -> collect_exp_reads read_set e
       | If (e, _, _, _, _) -> collect_exp_reads read_set e
       | Switch (e, _, _, _, _) -> collect_exp_reads read_set e
       | ComputedGoto (e, _) -> collect_exp_reads read_set e
       | _ -> ());
      DoChildren
  end in
  ignore (visitCilFunction vis fd);
  read_set

(* ------------------------------------------------------------------ *)
(*  Phase 2 — eliminate dead declarations, stores, and call results   *)
(* ------------------------------------------------------------------ *)

(** Returns true if vi.vid is a local (not formal) and not in the read set. *)
let is_dead_local (local_set : (int, bool) Hashtbl.t)
                  (read_set  : (int, bool) Hashtbl.t)
                  (vi : varinfo) : bool =
  Hashtbl.mem local_set vi.vid && not (Hashtbl.mem read_set vi.vid)

(** Eliminate dead stores/decls in one function.  Returns the number
    of variables removed (used for fixed-point detection). *)
let eliminate_dead (fd : fundec) : int =
  let read_set = collect_reads fd in
  (* Build local-only set (excludes formals) *)
  let local_set = Hashtbl.create 16 in
  List.iter (fun vi -> Hashtbl.replace local_set vi.vid true) fd.slocals;

  let dead vi = is_dead_local local_set read_set vi in

  (* Count how many locals we'll remove *)
  let dead_count = List.length (List.filter (fun vi -> dead vi) fd.slocals) in

  if dead_count = 0 then 0
  else begin
    (* Walk instructions and strip dead ones *)
    let vis = object
      inherit nopCilVisitor

      method! vinst (i : instr) : instr list visitAction =
        match i with
        | Set ((Var vi, _), _, _, _) when dead vi ->
          ChangeTo []  (* remove dead store *)
        | VarDecl (vi, _) when dead vi ->
          ChangeTo []  (* remove dead decl *)
        | Call (Some (Var vi, _), fn, args, loc1, loc2) when dead vi ->
          (* Keep the call for side effects, discard the result *)
          ChangeTo [Call (None, fn, args, loc1, loc2)]
        | _ -> SkipChildren
    end in
    ignore (visitCilFunction vis fd);

    (* Filter fd.slocals to remove dead variables *)
    fd.slocals <- List.filter (fun vi -> not (dead vi)) fd.slocals;

    dead_count
  end

(* ------------------------------------------------------------------ *)
(*  Entry point — iterate to fixed-point over all functions           *)
(* ------------------------------------------------------------------ *)

let apply (f : file) : unit =
  let max_iters = 20 in
  let iter = ref 0 in
  let changed = ref true in
  while !changed && !iter < max_iters do
    incr iter;
    changed := false;
    iterGlobals f (fun g ->
      match g with
      | GFun (fd, _) ->
        let removed = eliminate_dead fd in
        if removed > 0 then begin
          changed := true;
          Printf.eprintf "[remove_dead_locals] iter %d: %s — removed %d dead locals\n%!"
            !iter fd.svar.vname removed
        end
      | _ -> ()
    )
  done

let transform : Transform.t = {
  name = "Remove dead locals and stores";
  apply;
}
