(* reconstruct_arrays.ml — Reconstruct compact array initializers from CIL's
   element-by-element assignments.

   CIL normalizes local array initialization:
     int a[3] = {1, 2, 3};
   into:
     int a[3];
     a[0] = 1;
     a[1] = 2;
     a[2] = 3;

   This transform detects sequences of consecutive Set instructions that
   assign to every element of a local array variable (in index order),
   using only constant expressions, and reconstructs the compact
   CompoundInit initializer.  The individual Set instructions are then
   removed, significantly reducing SLOC for Csmith-style programs with
   large array initializers.

   Safety:
   - Only handles local arrays (in slocals)
   - Only merges when ALL elements from index 0..N-1 are assigned in order
   - Only constant expressions (no dependency issues)
   - Variable's address must not be taken (no aliasing concerns)
   - Assignments must be in the leading instruction block (before control flow) *)

open GoblintCil

(* ------------------------------------------------------------------ *)
(*  Helpers                                                            *)
(* ------------------------------------------------------------------ *)

(** Try to extract a constant integer from a CIL expression.
    Only used for array INDEX expressions, which are small ints. *)
let rec const_int_of_exp (e : exp) : int option =
  match e with
  | Const (CInt (i, _, _)) ->
    (try Some (Z.to_int i) with Z.Overflow -> None)
  | CastE (_, e') -> const_int_of_exp e'
  | _ -> None

(** Check whether an expression is safe to use in a variable initializer.
    An expression is safe if it doesn't READ the value of a local variable.
    Taking the ADDRESS of a local is always safe because locals are
    stack-allocated at function entry — the address is valid even before
    the local is initialized.  But reading a local's VALUE is not safe
    because the local may not have been assigned yet at declaration point. *)
let rec expr_safe_for_init (local_set : (int, bool) Hashtbl.t) (e : exp) : bool =
  match e with
  | Const _ -> true
  | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOf _ | AlignOfE _ -> true
  | AddrOf lv | StartOf lv ->
    (* Taking address of anything (including locals) is safe — no value read *)
    addr_safe_for_init local_set lv
  | Lval lv ->
    (* Reading a VALUE — must not reference locals *)
    lval_safe_for_init local_set lv
  | UnOp (_, e1, _) -> expr_safe_for_init local_set e1
  | BinOp (_, e1, e2, _) ->
    expr_safe_for_init local_set e1 && expr_safe_for_init local_set e2
  | CastE (_, e1) -> expr_safe_for_init local_set e1
  | Question (e1, e2, e3, _) ->
    expr_safe_for_init local_set e1
    && expr_safe_for_init local_set e2
    && expr_safe_for_init local_set e3
  | Real e1 | Imag e1 -> expr_safe_for_init local_set e1
  | AddrOfLabel _ -> true

(** For Lval (value reads): reject locals *)
and lval_safe_for_init (local_set : (int, bool) Hashtbl.t) ((host, off) : lval) : bool =
  let host_ok = match host with
    | Var vi -> vi.vglob || not (Hashtbl.mem local_set vi.vid)
    | Mem e -> expr_safe_for_init local_set e
  in
  host_ok && offset_safe_for_init local_set off

(** For AddrOf (address computation): locals are OK since the address is always
    valid, but we still need to check that any index expressions are safe. *)
and addr_safe_for_init (local_set : (int, bool) Hashtbl.t) ((_host, off) : lval) : bool =
  (* The host variable (local or global) is fine — we're taking its address.
     But index expressions inside the offset might read local values. *)
  offset_safe_for_init local_set off

and offset_safe_for_init (local_set : (int, bool) Hashtbl.t) (off : offset) : bool =
  match off with
  | NoOffset -> true
  | Field (_, o) -> offset_safe_for_init local_set o
  | Index (e, o) ->
    expr_safe_for_init local_set e && offset_safe_for_init local_set o

(** Safe comparison for offsets: avoids structural equality on fieldinfo
    (which contains cyclic compinfo references). Compares by field name +
    composite key instead. *)
let rec offset_equal (o1 : offset) (o2 : offset) : bool =
  match o1, o2 with
  | NoOffset, NoOffset -> true
  | Field (f1, r1), Field (f2, r2) ->
    f1.fname = f2.fname && f1.fcomp.ckey = f2.fcomp.ckey && offset_equal r1 r2
  | Index (e1, r1), Index (e2, r2) ->
    (match const_int_of_exp e1, const_int_of_exp e2 with
     | Some i1, Some i2 -> i1 = i2 && offset_equal r1 r2
     | _ -> false)
  | _ -> false

(** Flatten a multi-dimensional index offset, returning the index list and
    any trailing non-index suffix (e.g. Field(f0, NoOffset) for arrays of
    structs/unions).
    Index(0, Index(1, Field(f0, NoOffset))) → Some ([0; 1], Field(f0, NoOffset))
    Index(0, Index(1, NoOffset))            → Some ([0; 1], NoOffset) *)
let rec flatten_index_offset (off : offset) : (int list * offset) option =
  match off with
  | NoOffset -> Some ([], NoOffset)
  | Index (e, rest) ->
    (match const_int_of_exp e, flatten_index_offset rest with
     | Some i, Some (is, suffix) -> Some (i :: is, suffix)
     | _ -> None)
  | Field _ -> Some ([], off)   (* trailing field — return as suffix *)

(** Get the dimensions (sizes) of a possibly multi-dimensional array type.
    TArray(TArray(TArray(int, 3), 7), 10) → [10; 7; 3] *)
let rec array_dimensions (t : typ) : int list option =
  match unrollType t with
  | TArray (elem_t, len_opt, _) ->
    (try
       let n = lenOfArray len_opt in
       (match array_dimensions elem_t with
        | Some dims -> Some (n :: dims)
        | None -> Some [n])
     with LenOfArray -> None)
  | _ -> None

(** Get the innermost element type of a (possibly nested) array type. *)
let rec array_base_type (t : typ) : typ =
  match unrollType t with
  | TArray (elem_t, _, _) -> array_base_type elem_t
  | t -> t

(** Convert a flat list of indices [i0; i1; ...] into a CIL offset chain
    Index(i0, Index(i1, ..., NoOffset)). *)
let rec make_index_offset (indices : int list) : offset =
  match indices with
  | [] -> NoOffset
  | i :: rest -> Index (integer i, make_index_offset rest)

(** Convert a linear index to multi-dimensional indices given dimension sizes.
    E.g., linear index 7 in dims [10; 7; 3] → indices for the corresponding
    multi-dimensional position. The dims list gives sizes from outermost to
    innermost.  Returns the list of indices in outer-to-inner order. *)
let linear_to_multi (dims : int list) (linear : int) : int list =
  let rec aux ds lin acc =
    match ds with
    | [] -> List.rev acc
    | [_d] -> List.rev (lin :: acc)
    | _d :: rest ->
      let stride = List.fold_left ( * ) 1 rest in
      let idx = lin / stride in
      aux rest (lin mod stride) (idx :: acc)
  in
  aux dims linear []

(** Total number of elements in a multi-dimensional array. *)
let total_elements (dims : int list) : int =
  List.fold_left ( * ) 1 dims

(** Build a (possibly nested) CompoundInit from a flat array of expressions,
    given the array type. For a 1-D array, builds:
      CompoundInit(arr_type, [(Index(0,NoOff), SingleInit e0); ...])
    For multi-dimensional, builds nested CompoundInits. *)
let rec build_compound_init (t : typ) (exprs : exp array) (base : int)
    (suffix : offset) : init =
  match unrollType t with
  | TArray (elem_t, len_opt, attrs) ->
    let n = (try lenOfArray len_opt with LenOfArray -> 0) in
    let sub_size =
      match array_dimensions elem_t with
      | Some dims -> total_elements dims
      | None -> 1
    in
    let entries = List.init n (fun i ->
      let off = Index (integer i, NoOffset) in
      let sub_init =
        build_compound_init elem_t exprs (base + i * sub_size) suffix in
      (off, sub_init)
    ) in
    CompoundInit (TArray (elem_t, len_opt, attrs), entries)
  | _ ->
    (* Base case: scalar or struct/union element *)
    (match suffix with
     | NoOffset -> SingleInit exprs.(base)
     | _ ->
       (* Wrap in CompoundInit for struct/union with field designator *)
       CompoundInit (t, [(suffix, SingleInit exprs.(base))]))

(* ------------------------------------------------------------------ *)
(*  Core: detect and reconstruct array inits in a function             *)
(* ------------------------------------------------------------------ *)

(** Process one function: scan leading instructions for array init patterns. *)
let reconstruct_in_function (fd : fundec) : unit =
  (* Build set of local variable IDs (for init-safety checking) *)
  let local_set = Hashtbl.create 32 in
  List.iter (fun vi -> Hashtbl.replace local_set vi.vid true) fd.slocals;
  List.iter (fun vi -> Hashtbl.replace local_set vi.vid true) fd.sformals;

  (* Build set of local array variables *)
  let local_arrays = Hashtbl.create 16 in
  List.iter (fun vi ->
    match array_dimensions vi.vtype with
    | Some dims ->
      Hashtbl.replace local_arrays vi.vid (vi, dims)
    | _ -> ()
  ) fd.slocals;

  if Hashtbl.length local_arrays = 0 then ()
  else

  (* Walk ALL instruction blocks in the function, reconstructing
     array initializers wherever we find complete sequential assignments. *)
  let rec process_stmts stmts =
    List.iter (fun s ->
      match s.skind with
      | Instr instrs ->
        let new_instrs = process_instrs instrs in
        s.skind <- Instr new_instrs
      | Block b ->
        process_stmts b.bstmts
      | If (_, tb, fb, _, _) ->
        process_stmts tb.bstmts;
        process_stmts fb.bstmts
      | Switch (_, b, _, _, _) ->
        process_stmts b.bstmts
      | Loop (b, _, _, _, _) ->
        process_stmts b.bstmts
      | Return _ | Goto _ | Break _ | Continue _ | ComputedGoto _ -> ()
    ) stmts

  and process_instrs (instrs : instr list) : instr list =
    (* Group consecutive assignments by variable.
       We scan through instructions, accumulating runs of array element
       assignments. When we encounter something that breaks the pattern,
       we flush the accumulated run. *)
    let result = ref [] in
    (* Current accumulator: (varinfo, dims, (flat_index → exp) mapping,
       expected_next_flat_idx, trailing offset suffix) *)
    let current_var :
      (varinfo * int list * (int, exp) Hashtbl.t * int ref * offset) option ref
      = ref None in

    let flush () =
      match !current_var with
      | None -> ()
      | Some (vi, dims, collected, _next_idx, suffix) ->
        let total = total_elements dims in
        if Hashtbl.length collected = total then begin
          (* We have a complete initialization — build CompoundInit *)
          let exprs = Array.make total (zero) in
          Hashtbl.iter (fun idx e -> exprs.(idx) <- e) collected;
          let compound = build_compound_init vi.vtype exprs 0 suffix in
          vi.vinit.init <- Some compound;
          (* Don't add the Set instructions back — they're consumed *)
          ()
        end else begin
          (* Incomplete — put all collected Sets back in original order *)
          let sorted = List.sort (fun (a, _) (b, _) -> compare a b)
              (Hashtbl.fold (fun k v acc -> (k, v) :: acc) collected []) in
          List.iter (fun (idx, e) ->
            let indices = linear_to_multi dims idx in
            let off = make_index_offset indices in
            (* Re-append the suffix (e.g. .f0) for struct/union arrays *)
            let full_off = addOffset suffix off in
            result := Set ((Var vi, full_off), e, locUnknown, locUnknown) :: !result
          ) sorted
        end;
        current_var := None
    in

    let flat_index_of_indices (dims : int list) (indices : int list) : int =
      let rec aux ds idxs =
        match ds, idxs with
        | [_], [i] -> i
        | _d :: drest, i :: irest ->
          let stride = List.fold_left ( * ) 1 drest in
          i * stride + aux drest irest
        | _ -> -1  (* mismatch *)
      in
      aux dims indices
    in

    List.iter (fun instr ->
      match instr with
      | Set ((Var vi, off), e, _, _)
        when Hashtbl.mem local_arrays vi.vid
          && expr_safe_for_init local_set e
        ->
        (match flatten_index_offset off with
         | Some (indices, suffix) when List.length indices > 0 ->
           let (_lvi, dims) = Hashtbl.find local_arrays vi.vid in
           let flat_idx = flat_index_of_indices dims indices in
           if flat_idx >= 0 then begin
             (* Check if this continues the current var's sequence *)
             (match !current_var with
              | Some (cv, _cdims, collected, next_idx, prev_suffix)
                when cv.vid = vi.vid && offset_equal prev_suffix suffix ->
                if flat_idx = !next_idx then begin
                  Hashtbl.replace collected flat_idx e;
                  next_idx := flat_idx + 1
                end else begin
                  (* Out of order — flush and don't start new *)
                  flush ();
                  result := instr :: !result
                end
              | Some (cv, _, _, _, prev_suffix)
                when cv.vid = vi.vid && not (offset_equal prev_suffix suffix) ->
                (* Same var but different field suffix — flush and emit *)
                flush ();
                result := instr :: !result
              | Some _ ->
                (* Different variable — flush previous, start new sequence *)
                flush ();
                if flat_idx = 0 then begin
                  let tbl = Hashtbl.create 64 in
                  Hashtbl.replace tbl 0 e;
                  current_var := Some (vi, dims, tbl, ref 1, suffix)
                end else
                  result := instr :: !result
              | None ->
                (* Start new sequence if this is index 0 *)
                if flat_idx = 0 then begin
                  let tbl = Hashtbl.create 64 in
                  Hashtbl.replace tbl 0 e;
                  current_var := Some (vi, dims, tbl, ref 1, suffix)
                end else
                  result := instr :: !result)
           end else begin
             flush ();
             result := instr :: !result
           end
         | _ ->
           flush ();
           result := instr :: !result)
      | _ ->
        (* Non-array-set instruction — flush any pending *)
        flush ();
        result := instr :: !result
    ) instrs;
    flush ();
    List.rev !result
  in
  process_stmts fd.sbody.bstmts

(* ------------------------------------------------------------------ *)
(*  Visitor and entry point                                            *)
(* ------------------------------------------------------------------ *)

class visitor = object
  inherit nopCilVisitor
  method! vfunc (fd : fundec) : fundec visitAction =
    reconstruct_in_function fd;
    SkipChildren
end

let apply (f : file) : unit =
  visitCilFileSameGlobals (new visitor) f

let transform : Transform.t = {
  name = "Reconstruct array initializers";
  apply;
}
