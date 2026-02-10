(* remove_header_globals.ml — Strip globals that came from #include'd headers *)
(* After gcc -E (without -P), CIL's lexer reads the # line directives and
   sets location.file for every global.  Globals whose location.file does
   not match the original source file (the one the user actually wrote)
   are header expansions and should be removed.
   We also keep globals with locUnknown (e.g. compiler builtins that CIL
   synthesises) — they are harmless and some may be needed. *)

open GoblintCil

(** Global ref set by slicer.ml before the pipeline runs.
    Holds the path to the *original* (pre-preprocessed) source file,
    or None when the slicer is invoked without it. *)
let original_source : string option ref = ref None

(** Decide whether a global belongs to user code (keep) or a header (drop).
    Strategy: compare the basename of the global's file with the basename
    of the original source.  We use basenames because gcc -E may emit
    absolute paths, relative paths, or just the filename depending on flags. *)
let is_user_global (src_basename : string) (g : global) : bool =
  let loc = get_globalLoc g in
  if loc == locUnknown || loc.file = "" then
    true  (* keep unknowns — they are CIL-synthesised *)
  else
    let g_base = Filename.basename loc.file in
    (* The global is "ours" if its file basename matches the source basename *)
    g_base = src_basename

let apply (f : file) : unit =
  match !original_source with
  | None ->
    (* No original source given — skip this transform *)
    Printf.eprintf "[slicer]   (no original source path — skipping header removal)\n%!"
  | Some src_path ->
    let src_basename = Filename.basename src_path in
    let before = List.length f.globals in
    f.globals <- List.filter (is_user_global src_basename) f.globals;
    let after = List.length f.globals in
    Printf.eprintf "[slicer]   Removed %d header globals (kept %d)\n%!" (before - after) after

let transform : Transform.t = {
  name = "Remove header globals";
  apply;
}
