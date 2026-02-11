(* remove_dead_calls.ml — Remove pure function calls with discarded results *)
(* Csmith's safe_* wrappers (safe_add_func_int32_t_s_s, etc.) are pure
   functions: they perform overflow-safe arithmetic, have no side effects
   (no global writes, no I/O), and return a computed value.

   When such a call appears as a statement with its return value discarded
   — i.e.  Call(None, safe_*(...), args, ...) — the entire instruction is
   dead and can be removed.

   This pass runs after Remove_dead_locals, which may have already
   rewritten  Call(Some dead_var, safe_*(...), ...)  →  Call(None, ...),
   creating additional opportunities. *)

open GoblintCil

(** Returns true if the function name matches a known-pure csmith wrapper.
    All safe_* functions in csmith's safe_math.h are pure. *)
let is_pure_safe_fn (name : string) : bool =
  (* All csmith safe math wrappers start with "safe_" *)
  let len = String.length name in
  len >= 5 && String.sub name 0 5 = "safe_"

class visitor = object
  inherit nopCilVisitor

  method! vinst (i : instr) : instr list visitAction =
    match i with
    | Call (None, Lval (Var vi, NoOffset), _args, _, _)
      when is_pure_safe_fn vi.vname ->
      ChangeTo []  (* remove the dead pure call *)
    | _ -> SkipChildren
end

let apply (f : file) : unit =
  let count = ref 0 in
  (* Count before removal for logging *)
  let counter = object
    inherit nopCilVisitor
    method! vinst (i : instr) : instr list visitAction =
      (match i with
       | Call (None, Lval (Var vi, NoOffset), _, _, _)
         when is_pure_safe_fn vi.vname -> incr count
       | _ -> ());
      SkipChildren
  end in
  visitCilFileSameGlobals counter f;
  if !count > 0 then begin
    visitCilFileSameGlobals (new visitor) f;
    Printf.eprintf "[remove_dead_calls] Removed %d dead pure safe_* calls\n%!" !count
  end

let transform : Transform.t = {
  name = "Remove dead pure calls";
  apply;
}
