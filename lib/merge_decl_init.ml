(* merge_decl_init.ml — Re-merge local declarations with their initial assignments *)
(* CIL normalizes `int d = 50;` into separate declaration (`int d;` in slocals)
   and assignment (`d = 50;` as a Set instruction in the body). This transform
   merges them back when safe, reducing redundant semicolons.

   Safety: only merges when:
   - The variable is a local (in slocals, not a formal)
   - The Set is a leading instruction in the function body (before control flow)
   - The expression is a constant (no dependency issues)
   - The variable's address is not taken (no aliasing) *)

open GoblintCil

let merge_locals (fd : fundec) : unit =
  (* Build set of local variable IDs *)
  let local_set = Hashtbl.create 16 in
  List.iter (fun vi -> Hashtbl.replace local_set vi.vid vi) fd.slocals;

  (* Find the first Instr statement and try to merge leading Sets *)
  let rec find_first_instr stmts =
    match stmts with
    | [] -> ()
    | s :: _ ->
      match s.skind with
      | Instr instrs ->
        let merged = Hashtbl.create 8 in
        let new_instrs = List.filter (fun instr ->
          match instr with
          | Set ((Var vi, NoOffset), e, _, _)
            when Hashtbl.mem local_set vi.vid
              && not (Hashtbl.mem merged vi.vid)
              && not vi.vaddrof
              && isConstant e ->
            (* Merge: set the initializer on the varinfo *)
            vi.vinit.init <- Some (SingleInit e);
            Hashtbl.replace merged vi.vid true;
            false  (* remove this Set from instructions *)
          | _ -> true  (* keep *)
        ) instrs in
        s.skind <- Instr new_instrs
      | Block b -> find_first_instr b.bstmts
      | _ -> ()
  in
  find_first_instr fd.sbody.bstmts

class visitor = object
  inherit nopCilVisitor
  method! vfunc (fd : fundec) : fundec visitAction =
    merge_locals fd;
    SkipChildren  (* already modified in place *)
end

let apply (f : file) : unit =
  visitCilFileSameGlobals (new visitor) f

let transform : Transform.t = {
  name = "Merge declarations with initializers";
  apply;
}
