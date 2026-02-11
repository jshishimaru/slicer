(* inline_temps.ml — Inline single-use CIL temporary variables.

   CIL normalizes complex expressions by introducing temporary variables:
     tmp = safe_func(args);
     x = tmp + 1;
   This transform detects single-use temporaries and inlines them:
     x = safe_func(args) + 1;

   This reduces SLOC significantly for Csmith programs processed through CIL,
   since CIL introduces a temporary variable for every sub-expression. *)

open GoblintCil

(* ------------------------------------------------------------------ *)
(*  Counting variable references                                       *)
(* ------------------------------------------------------------------ *)

type var_usage = {
  mutable reads : int;   (* times the variable's VALUE is read *)
  mutable writes : int;  (* times the variable is assigned *)
}

(** Count substitutable references of variable [vid] within an expression.
    Only counts `Lval(Var vi, NoOffset)` — the exact pattern that can be
    textually substituted.  AddrOf, StartOf, field accesses, and index
    accesses involving the variable are NOT counted because they cannot
    be replaced by the RHS expression. *)
let rec count_subst_refs (vid : int) (e : exp) : int =
  match e with
  | Lval (Var vi, NoOffset) when vi.vid = vid -> 1
  | Lval (Var _, _) -> 0  (* same var with non-NoOffset: can't substitute *)
  | Lval (Mem addr, off) ->
    count_subst_refs vid addr + count_subst_refs_offset vid off
  | AddrOf (Mem addr, off) | StartOf (Mem addr, off) ->
    count_subst_refs vid addr + count_subst_refs_offset vid off
  | AddrOf (Var _, _) | StartOf (Var _, _) -> 0  (* address-of: can't inline *)
  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AddrOfLabel _ -> 0
  | SizeOfE e1 | AlignOfE e1 | UnOp (_, e1, _) | CastE (_, e1)
  | Real e1 | Imag e1 ->
    count_subst_refs vid e1
  | BinOp (_, e1, e2, _) ->
    count_subst_refs vid e1 + count_subst_refs vid e2
  | Question (e1, e2, e3, _) ->
    count_subst_refs vid e1 + count_subst_refs vid e2
    + count_subst_refs vid e3

and count_subst_refs_offset (vid : int) (off : offset) : int =
  match off with
  | NoOffset -> 0
  | Field (_, o) -> count_subst_refs_offset vid o
  | Index (e, o) -> count_subst_refs vid e + count_subst_refs_offset vid o

(** Count ALL references of variable [vid] in an expression (any position). *)
let rec count_all_refs (vid : int) (e : exp) : int =
  match e with
  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AddrOfLabel _ -> 0
  | Lval lv -> count_all_refs_lval vid lv
  | AddrOf lv | StartOf lv -> count_all_refs_lval vid lv
  | SizeOfE e1 | AlignOfE e1 | UnOp (_, e1, _) | CastE (_, e1)
  | Real e1 | Imag e1 ->
    count_all_refs vid e1
  | BinOp (_, e1, e2, _) ->
    count_all_refs vid e1 + count_all_refs vid e2
  | Question (e1, e2, e3, _) ->
    count_all_refs vid e1 + count_all_refs vid e2
    + count_all_refs vid e3

and count_all_refs_lval (vid : int) ((host, off) : lval) : int =
  let h = match host with
    | Var vi -> if vi.vid = vid then 1 else 0
    | Mem e -> count_all_refs vid e
  in
  h + count_all_refs_offset vid off

and count_all_refs_offset (vid : int) (off : offset) : int =
  match off with
  | NoOffset -> 0
  | Field (_, o) -> count_all_refs_offset vid o
  | Index (e, o) -> count_all_refs vid e + count_all_refs_offset vid o

(** Count substitutable reads and writes of [vid] in an instruction.
    Only counts Lval(Var vi, NoOffset) as a substitutable read.
    Returns (subst_reads, writes, total_all_refs). *)
let count_in_instr (vid : int) (instr : instr) : int * int * int =
  match instr with
  | Set ((Var vi, off), rhs, _, _) ->
    let w = if vi.vid = vid then 1 else 0 in
    let off_sub = count_subst_refs_offset vid off in
    let rhs_sub = count_subst_refs vid rhs in
    let off_all = count_all_refs_offset vid off in
    let rhs_all = count_all_refs vid rhs in
    (off_sub + rhs_sub, w, off_all + rhs_all)
  | Set ((Mem addr, off), rhs, _, _) ->
    let sub = count_subst_refs vid addr + count_subst_refs_offset vid off
              + count_subst_refs vid rhs in
    let all = count_all_refs vid addr + count_all_refs_offset vid off
              + count_all_refs vid rhs in
    (sub, 0, all)
  | Call (ret_opt, fn, args, _, _) ->
    let ret_w, ret_sub, ret_all = match ret_opt with
      | Some (Var vi, off) ->
        let w = if vi.vid = vid then 1 else 0 in
        (w, count_subst_refs_offset vid off, count_all_refs_offset vid off)
      | Some (Mem addr, off) ->
        (0, count_subst_refs vid addr + count_subst_refs_offset vid off,
         count_all_refs vid addr + count_all_refs_offset vid off)
      | None -> (0, 0, 0)
    in
    let fn_sub = count_subst_refs vid fn in
    let fn_all = count_all_refs vid fn in
    let arg_sub =
      List.fold_left (fun acc a -> acc + count_subst_refs vid a) 0 args in
    let arg_all =
      List.fold_left (fun acc a -> acc + count_all_refs vid a) 0 args in
    (ret_sub + fn_sub + arg_sub, ret_w, ret_all + fn_all + arg_all)
  | VarDecl _ | Asm _ -> (0, 0, 0)

(** Count total substitutable reads, writes, and all refs across all stmts. *)
let count_usage_in_stmts (vid : int) (stmts : stmt list) : int * int * int =
  let total_sub = ref 0 in
  let total_w = ref 0 in
  let total_all = ref 0 in
  let rec walk_stmts ss =
    List.iter (fun s ->
      match s.skind with
      | Instr instrs ->
        List.iter (fun i ->
          let (sub, w, all) = count_in_instr vid i in
          total_sub := !total_sub + sub;
          total_w := !total_w + w;
          total_all := !total_all + all
        ) instrs
      | Block b -> walk_stmts b.bstmts
      | If (cond, tb, fb, _, _) ->
        total_sub := !total_sub + count_subst_refs vid cond;
        total_all := !total_all + count_all_refs vid cond;
        walk_stmts tb.bstmts;
        walk_stmts fb.bstmts
      | Switch (e, b, _, _, _) ->
        total_sub := !total_sub + count_subst_refs vid e;
        total_all := !total_all + count_all_refs vid e;
        walk_stmts b.bstmts
      | Loop (b, _, _, _, _) ->
        walk_stmts b.bstmts
      | Return (Some e, _, _) ->
        total_sub := !total_sub + count_subst_refs vid e;
        total_all := !total_all + count_all_refs vid e
      | Return (None, _, _) | Goto _ | Break _ | Continue _
      | ComputedGoto _ -> ()
    ) ss
  in
  walk_stmts stmts;
  (!total_sub, !total_w, !total_all)

(* ------------------------------------------------------------------ *)
(*  Expression substitution                                            *)
(* ------------------------------------------------------------------ *)

(** Substitute all occurrences of Lval(Var vid, NoOffset) → replacement
    in an expression. *)
let rec subst_exp (vid : int) (repl : exp) (e : exp) : exp =
  match e with
  | Lval (Var vi, NoOffset) when vi.vid = vid -> repl
  | Lval lv -> Lval (subst_lval vid repl lv)
  | AddrOf lv -> AddrOf (subst_lval vid repl lv)
  | StartOf lv -> StartOf (subst_lval vid repl lv)
  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AddrOfLabel _ -> e
  | SizeOfE e1 -> SizeOfE (subst_exp vid repl e1)
  | AlignOfE e1 -> AlignOfE (subst_exp vid repl e1)
  | UnOp (op, e1, t) -> UnOp (op, subst_exp vid repl e1, t)
  | BinOp (op, e1, e2, t) ->
    BinOp (op, subst_exp vid repl e1, subst_exp vid repl e2, t)
  | CastE (t, e1) -> CastE (t, subst_exp vid repl e1)
  | Question (e1, e2, e3, t) ->
    Question (subst_exp vid repl e1, subst_exp vid repl e2,
              subst_exp vid repl e3, t)
  | Real e1 -> Real (subst_exp vid repl e1)
  | Imag e1 -> Imag (subst_exp vid repl e1)

and subst_lval (vid : int) (repl : exp) ((host, off) : lval) : lval =
  let host' = match host with
    | Var _ -> host  (* Don't substitute in var position — 
                        only in Lval(Var, NoOffset) which is caught above *)
    | Mem e -> Mem (subst_exp vid repl e)
  in
  (host', subst_offset vid repl off)

and subst_offset (vid : int) (repl : exp) (off : offset) : offset =
  match off with
  | NoOffset -> NoOffset
  | Field (fi, o) -> Field (fi, subst_offset vid repl o)
  | Index (e, o) -> Index (subst_exp vid repl e, subst_offset vid repl o)

(** Substitute in an instruction (all expression positions). *)
let subst_instr (vid : int) (repl : exp) (instr : instr) : instr =
  match instr with
  | Set (lv, rhs, loc1, loc2) ->
    Set (subst_lval vid repl lv, subst_exp vid repl rhs, loc1, loc2)
  | Call (ret, fn, args, loc1, loc2) ->
    let ret' = match ret with
      | Some lv -> Some (subst_lval vid repl lv)
      | None -> None
    in
    Call (ret', subst_exp vid repl fn,
          List.map (subst_exp vid repl) args, loc1, loc2)
  | VarDecl _ | Asm _ -> instr

(* ------------------------------------------------------------------ *)
(*  Peephole inlining within instruction blocks                        *)
(* ------------------------------------------------------------------ *)

(** Process one function: inline single-use CIL temporaries. *)
let inline_in_function (fd : fundec) : unit =
  (* Step 1: find candidate temps — locals with exactly 1 write and 1 read,
     assigned via Set((Var vi, NoOffset), rhs, ...) *)
  let candidates = Hashtbl.create 32 in
  List.iter (fun (vi : varinfo) ->
    (* Only consider local variables that:
       - are not global or formal
       - have their address never taken (vaddrof = false)
       - have exactly 1 write and 1 substitutable read
       - have no other (non-substitutable) references *)
    if not vi.vglob && not vi.vaddrof then begin
      let (subst_reads, writes, all_refs) =
        count_usage_in_stmts vi.vid fd.sbody.bstmts in
      if writes = 1 && subst_reads = 1 && all_refs = subst_reads then
        Hashtbl.replace candidates vi.vid true
    end
  ) fd.slocals;

  if Hashtbl.length candidates = 0 then ()
  else begin
    (* Step 2: peephole pass over instruction blocks.
       For each consecutive pair where:
       - First is Set((Var tmp, NoOffset), rhs, ...) with tmp in candidates
       - Second uses tmp exactly once
       Inline rhs into the second and remove the first. *)
    let changed = ref true in

    (* We iterate until fixpoint to handle chains like:
         tmp1 = expr; tmp2 = f(tmp1); x = tmp2;
       After inlining tmp2, we might inline tmp1 on the next pass. *)
    while !changed do
      changed := false;

      let rec process_stmts stmts =
        List.iter (fun s ->
          match s.skind with
          | Instr instrs ->
            let new_instrs = process_instrs instrs in
            if List.length new_instrs <> List.length instrs then begin
              s.skind <- Instr new_instrs;
              changed := true
            end
          | Block b -> process_stmts b.bstmts
          | If (_, tb, fb, _, _) ->
            process_stmts tb.bstmts;
            process_stmts fb.bstmts
          | Switch (_, b, _, _, _) -> process_stmts b.bstmts
          | Loop (b, _, _, _, _) -> process_stmts b.bstmts
          | Return _ | Goto _ | Break _ | Continue _
          | ComputedGoto _ -> ()
        ) stmts

      and process_instrs (instrs : instr list) : instr list =
        match instrs with
        | [] -> []
        | [single] -> [single]
        | first :: rest ->
          (match first with
           (* Pattern 1: Set(tmp, rhs) followed by use → substitute rhs *)
           | Set ((Var vi, NoOffset), rhs, _, _)
             when Hashtbl.mem candidates vi.vid ->
             let next = List.hd rest in
             let (sub_refs, _, all_refs) = count_in_instr vi.vid next in
             if sub_refs = 1 && all_refs = sub_refs then begin
               let inlined = subst_instr vi.vid rhs next in
               Hashtbl.remove candidates vi.vid;
               process_instrs (inlined :: List.tl rest)
             end else
               first :: process_instrs rest

           (* Pattern 2: Call(tmp, func, args) followed by
              Set(lv, Lval(Var tmp, NoOffset)) → redirect Call result *)
           | Call (Some (Var vi, NoOffset), fn, args, loc1, loc2)
             when Hashtbl.mem candidates vi.vid ->
             let next = List.hd rest in
             (match next with
              | Set (lv, Lval (Var vi2, NoOffset), _, _)
                when vi2.vid = vi.vid
                  && count_all_refs_lval vi.vid lv = 0 ->
                (* Redirect Call return target from tmp to lv *)
                let redirected = Call (Some lv, fn, args, loc1, loc2) in
                Hashtbl.remove candidates vi.vid;
                process_instrs (redirected :: List.tl rest)
              | _ ->
                first :: process_instrs rest)

           | _ ->
             first :: process_instrs rest)
      in
      process_stmts fd.sbody.bstmts
    done;

    (* Step 3: remove declarations of inlined temps from slocals.
       A temp was inlined if it was removed from candidates (i.e., no longer
       in the table) AND has 0 total references now. *)
    fd.slocals <- List.filter (fun vi ->
      if Hashtbl.mem candidates vi.vid then
        true  (* wasn't inlined — keep *)
      else begin
        let (_sub, writes, all) =
          count_usage_in_stmts vi.vid fd.sbody.bstmts in
        writes + all > 0  (* keep if still referenced *)
      end
    ) fd.slocals
  end

(* ------------------------------------------------------------------ *)
(*  Visitor and entry point                                            *)
(* ------------------------------------------------------------------ *)

class visitor = object
  inherit nopCilVisitor
  method! vfunc (fd : fundec) : fundec visitAction =
    inline_in_function fd;
    SkipChildren
end

let apply (f : file) : unit =
  visitCilFileSameGlobals (new visitor) f

let transform : Transform.t = {
  name = "Inline single-use temporaries";
  apply;
}
