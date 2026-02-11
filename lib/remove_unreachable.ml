(* remove_unreachable.ml — Dead-code elimination: remove unreachable statements
 *
 * Removes:
 *   - Code after Return/Break/Continue/Goto in a block (unreachable tail)
 *     BUT preserves any statement that carries a label (goto target)
 *   - Zero-trip loops: while(constant-false) { ... } → removed entirely
 *   - Constant-true if: if(const-true) { A } else { B } → A  (drop B)
 *   - Constant-false if: if(const-false) { A } else { B } → B  (drop A)
 *   - Empty Instr [] and Block { bstmts = [] } wrappers
 *
 * Safe: unreachable code by definition cannot affect behavior.
 * CPU-neutral: removed code was never executed anyway.
 *)

open GoblintCil

(* ── Does this statement carry any labels? ──
   If so, it might be a goto target and must not be removed. *)
let has_labels (s : stmt) : bool =
  s.labels <> []

(* ── Check if a statement unconditionally transfers control ──
   Returns true if control NEVER falls through this statement. *)
let rec is_terminal (s : stmt) : bool =
  match s.skind with
  | Return _ -> true
  | Break _ -> true
  | Continue _ -> true
  | Goto _ -> true
  | ComputedGoto _ -> true
  | Block b -> block_is_terminal b
  | If (_, tb, fb, _, _) ->
    (* Terminal only if BOTH branches are terminal *)
    block_is_terminal tb && block_is_terminal fb
  | Switch _ -> false
  | Loop _ -> false
  | Instr _ -> false

and block_is_terminal (b : block) : bool =
  let rec check = function
    | [] -> false
    | s :: _ when is_terminal s -> true
    | _ :: rest -> check rest
  in
  check b.bstmts

(* ── Evaluate constant integer expressions ── *)
let rec eval_const_expr (e : exp) : Cilint.cilint option =
  match e with
  | Const (CInt (i, _, _)) -> Some i
  | UnOp (LNot, e1, _) ->
    (match eval_const_expr e1 with
     | Some v ->
       Some (if Cilint.is_zero_cilint v then Cilint.one_cilint
             else Cilint.zero_cilint)
     | None -> None)
  | BinOp (Eq, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if Cilint.compare_cilint v1 v2 = 0 then Cilint.one_cilint
             else Cilint.zero_cilint)
     | _ -> None)
  | BinOp (Ne, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if Cilint.compare_cilint v1 v2 <> 0 then Cilint.one_cilint
             else Cilint.zero_cilint)
     | _ -> None)
  | BinOp (Lt, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if Cilint.compare_cilint v1 v2 < 0 then Cilint.one_cilint
             else Cilint.zero_cilint)
     | _ -> None)
  | BinOp (Le, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if Cilint.compare_cilint v1 v2 <= 0 then Cilint.one_cilint
             else Cilint.zero_cilint)
     | _ -> None)
  | BinOp (Gt, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if Cilint.compare_cilint v1 v2 > 0 then Cilint.one_cilint
             else Cilint.zero_cilint)
     | _ -> None)
  | BinOp (Ge, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if Cilint.compare_cilint v1 v2 >= 0 then Cilint.one_cilint
             else Cilint.zero_cilint)
     | _ -> None)
  | BinOp (LAnd, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if not (Cilint.is_zero_cilint v1) && not (Cilint.is_zero_cilint v2)
             then Cilint.one_cilint else Cilint.zero_cilint)
     | _ -> None)
  | BinOp (LOr, e1, e2, _) ->
    (match eval_const_expr e1, eval_const_expr e2 with
     | Some v1, Some v2 ->
       Some (if not (Cilint.is_zero_cilint v1) || not (Cilint.is_zero_cilint v2)
             then Cilint.one_cilint else Cilint.zero_cilint)
     | _ -> None)
  | CastE (_, e1) -> eval_const_expr e1
  | _ -> None

let is_constant_false (e : exp) : bool =
  match eval_const_expr e with
  | Some v -> Cilint.is_zero_cilint v
  | None -> false

let is_constant_true (e : exp) : bool =
  match eval_const_expr e with
  | Some v -> not (Cilint.is_zero_cilint v)
  | None -> false

(* ── Does a statement (or its children) contain any labels? ── *)
let rec stmt_has_labels_deep (s : stmt) : bool =
  if has_labels s then true
  else match s.skind with
  | Block b | Loop (b, _, _, _, _) -> block_has_labels_deep b
  | If (_, tb, fb, _, _) -> block_has_labels_deep tb || block_has_labels_deep fb
  | Switch (_, b, _, _, _) -> block_has_labels_deep b
  | _ -> false

and block_has_labels_deep (b : block) : bool =
  List.exists stmt_has_labels_deep b.bstmts

(* ── Count semicolon-LOC in a statement (for logging) ── *)
let rec count_sloc_stmt (s : stmt) : int =
  match s.skind with
  | Instr il -> List.length il
  | Return _ -> 1
  | If (_, tb, fb, _, _) -> count_sloc_block tb + count_sloc_block fb
  | Switch (_, b, _, _, _) -> count_sloc_block b
  | Loop (b, _, _, _, _) -> count_sloc_block b
  | Block b -> count_sloc_block b
  | Break _ | Continue _ | Goto _ | ComputedGoto _ -> 0

and count_sloc_block (b : block) : int =
  List.fold_left (fun acc s -> acc + count_sloc_stmt s) 0 b.bstmts

(* ── The main visitor ── *)
class removeUnreachableVisitor (removed : int ref) = object
  inherit nopCilVisitor

  method! vblock (b : block) =
    (* ── Pass 1: Truncate after terminal statements ──
       IMPORTANT: Never remove a statement that has labels (goto targets)
       or contains labels in its children. If we hit such a statement
       in the "dead" zone, we must keep it (and everything after it,
       since we can't know what follows a goto target). *)
    let new_stmts = ref [] in
    let found_terminal = ref false in
    let stopped_removing = ref false in
    List.iter (fun s ->
      if !stopped_removing then
        (* Already found a labeled stmt in dead zone; keep everything *)
        new_stmts := s :: !new_stmts
      else if !found_terminal then begin
        (* We're in the dead zone after a terminal *)
        if stmt_has_labels_deep s then begin
          (* This dead statement has a label — a goto might jump here.
             We must keep it and stop removing. *)
          new_stmts := s :: !new_stmts;
          stopped_removing := true
        end else begin
          let n = count_sloc_stmt s in
          if n > 0 then removed := !removed + n
        end
      end else begin
        new_stmts := s :: !new_stmts;
        if is_terminal s then found_terminal := true
      end
    ) b.bstmts;
    let new_stmts = List.rev !new_stmts in

    (* ── Pass 2: Remove zero-trip loops, prune constant branches,
       drop empty wrappers ── *)
    let new_stmts = List.filter_map (fun s ->
      match s.skind with
      (* Zero-trip loop: CIL while(cond) → Loop({ If(cond, {}, {Break}) ; body })
         If the condition is constant-false, the loop never executes.
         Only safe if loop body has no labels. *)
      | Loop (body, _, _, _, _) ->
        (match body.bstmts with
         | { skind = If (cond, _, _, _, _); _ } :: _
           when is_constant_false cond && not (block_has_labels_deep body) ->
           let n = count_sloc_block body in
           if n > 0 then removed := !removed + n;
           None
         | _ -> Some s)

      (* Constant-true if: keep only true branch.
         Only drop false branch if it has no labels. *)
      | If (cond, tb, fb, _, _) when is_constant_true cond
                                      && not (block_has_labels_deep fb) ->
        let n_fb = count_sloc_block fb in
        if n_fb > 0 then removed := !removed + n_fb;
        if tb.bstmts = [] then None
        else begin s.skind <- Block tb; Some s end

      (* Constant-false if: keep only false branch.
         Only drop true branch if it has no labels. *)
      | If (cond, tb, fb, _, _) when is_constant_false cond
                                      && not (block_has_labels_deep tb) ->
        let n_tb = count_sloc_block tb in
        if n_tb > 0 then removed := !removed + n_tb;
        if fb.bstmts = [] then None
        else begin s.skind <- Block fb; Some s end

      (* Drop empty instruction lists and empty blocks *)
      | Instr [] -> None
      | Block { bstmts = []; _ } -> None

      | _ -> Some s
    ) new_stmts in

    b.bstmts <- new_stmts;
    DoChildren
end

(* ── Entry point ── *)
let apply (f : file) : unit =
  let removed = ref 0 in
  let vis = new removeUnreachableVisitor removed in
  visitCilFileSameGlobals vis f;
  if !removed > 0 then
    Printf.eprintf "[remove_unreachable] Eliminated %d unreachable semicolon LOC\n%!"
      !removed

let transform : Transform.t = {
  name = "Remove unreachable code";
  apply;
}
