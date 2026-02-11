(* slicer.ml — Orchestrator for CIL-based C reducer *)
(* Parses a preprocessed C file, applies the transformation pipeline
   defined in Slicer_transforms.Transforms, and emits reduced C.
   Also collects before/after semicolon-LOC stats.
   All transformations live in lib/ — this file is purely plumbing. *)

open GoblintCil

let () =
  (* Parse command line *)
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: slicer <input.c> <output.c> [stats.txt] [--gcov-json <path>]\n";
    exit 1
  end;
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in

  (* Scan for named flags first, then treat remaining positional args *)
  let gcov_json_path = ref None in
  let skip_next = ref false in
  let positional = ref [] in
  for i = 3 to Array.length Sys.argv - 1 do
    if !skip_next then
      skip_next := false
    else if Sys.argv.(i) = "--gcov-json" && i + 1 < Array.length Sys.argv then begin
      gcov_json_path := Some Sys.argv.(i + 1);
      skip_next := true
    end else
      positional := Sys.argv.(i) :: !positional
  done;
  let positional = List.rev !positional in
  let stats_file = match positional with x :: _ -> Some x | [] -> None in
  let original_source = match positional with _ :: x :: _ -> Some x | _ -> None in

  (* Tell the header-stripping transforms which file is the "real" source *)
  Slicer_transforms.Remove_header_globals.original_source := original_source;

  (* Load gcov coverage data if provided *)
  (match !gcov_json_path with
   | Some path ->
     Printf.eprintf "[slicer] Loading gcov data from %s ...\n%!" path;
     let data = Slicer_transforms.Gcov_data.load path in
     Slicer_transforms.Remove_gcov_dead.gcov_data_ref := Some data
   | None ->
     Printf.eprintf "[slicer] No gcov data provided (use --gcov-json <path>)\n%!");

  (* Initialize CIL *)
  initCIL ();

  (* Step 1: Parse preprocessed C into CIL AST *)
  Printf.eprintf "[slicer] Parsing %s ...\n%!" input_file;
  let cil_file = Frontc.parse input_file () in

  (* Collect pre-transform stats (CIL AST — for debug logging only) *)
  let before_ast_stats = Slicer_transforms.Stats.collect cil_file in
  Printf.eprintf "[slicer] Before (CIL AST): %s\n%!" (Slicer_transforms.Stats.to_string before_ast_stats);

  (* Step 2: Apply transformation pipeline *)
  Slicer_transforms.Transforms.apply_all cil_file;

  (* Collect post-transform stats (CIL AST — for debug logging only) *)
  let after_ast_stats = Slicer_transforms.Stats.collect cil_file in
  Printf.eprintf "[slicer] After (CIL AST):  %s\n%!" (Slicer_transforms.Stats.to_string after_ast_stats);

  (* Step 3: Emit reduced C *)
  Printf.eprintf "[slicer] Writing reduced C to %s ...\n%!" output_file;
  Cil.lineDirectiveStyle := None;
  (* Use a temporary file to capture dumpFile output, then clean it *)
  let tmp_path = output_file ^ ".tmp" in
  let tmp_oc = open_out tmp_path in
  dumpFile defaultCilPrinter tmp_oc output_file cil_file;
  close_out tmp_oc;
  let raw = In_channel.with_open_text tmp_path In_channel.input_all in
  Sys.remove tmp_path;
  let cleaned = Slicer_transforms.Cleanup_output.cleanup raw in
  let oc = open_out output_file in
  output_string oc cleaned;
  close_out oc;

  (* Step 4: Write stats file if requested *)
  (* Count raw semicolons in original source (if provided) and final output *)
  let count_semicolons_in_file path =
    let ic = open_in path in
    let count = ref 0 in
    (try while true do
       let c = input_char ic in
       if c = ';' then incr count
     done with End_of_file -> ());
    close_in ic;
    !count
  in
  (match stats_file with
   | Some path ->
     let before_sloc = match original_source with
       | Some src -> count_semicolons_in_file src
       | None ->
         (* Fallback: count in the preprocessed input *)
         count_semicolons_in_file input_file
     in
     let after_sloc = count_semicolons_in_file output_file in
     let sc = open_out path in
     Printf.fprintf sc "before_semicolon_loc=%d\n" before_sloc;
     Printf.fprintf sc "after_semicolon_loc=%d\n" after_sloc;
     close_out sc;
     Printf.eprintf "[slicer] Stats written to %s (before=%d, after=%d)\n%!" path before_sloc after_sloc
   | None -> ());

  Printf.eprintf "[slicer] Done.\n%!"
