(* transforms.ml — Registry of all available transformations *)
(* Transformations are applied in the order listed here.
   To add a new transformation, append it to the list below. *)

(** The default transformation pipeline, applied in order. *)
let pipeline : Transform.t list = [
  Fold_constants.transform;
  Remove_gcov_dead.transform;    (* gcov-proven dead code: functions + statements *)
  Remove_unused.transform;
  Remove_empty_functions.transform;
  Merge_decl_init.transform;
  Remove_dead_locals.transform;  (* remove unused locals & dead stores *)
  Remove_dead_calls.transform;   (* remove pure safe_* calls with discarded results *)
  Remove_unused.transform;  (* second pass: catch newly unreachable code *)
  Reconstruct_arrays.transform;  (* re-merge element-by-element array inits *)
  (* NOTE: Remove_unreachable is available but disabled due to a label-preservation
     bug with CIL-generated goto targets (__Cont, _L). Can be re-enabled once fixed. *)
  Remove_header_globals.transform;  (* strip header-expanded globals *)
  Add_includes.transform;           (* re-add #include directives *)
]

(** Apply all transformations in [pipeline] to a CIL file, with logging. *)
let apply_all (f : GoblintCil.file) : unit =
  List.iter (fun (t : Transform.t) ->
    Printf.eprintf "[slicer] Applying: %s ...\n%!" t.name;
    t.apply f
  ) pipeline
