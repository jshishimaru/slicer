(* slicer.ml — Orchestrator for CIL-based C reducer *)
(* Parses a preprocessed C file, applies the transformation pipeline
   defined in Slicer_transforms.Transforms, and emits reduced C.
   Also collects before/after semicolon-LOC stats.
   All transformations live in lib/ — this file is purely plumbing. *)

open GoblintCil

let () =
  (* Parse command line *)
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: slicer <input.c> <output.c> [stats.txt] [original.c]\n";
    exit 1
  end;
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let stats_file = if Array.length Sys.argv > 3 then Some Sys.argv.(3) else None in
  let original_source = if Array.length Sys.argv > 4 then Some Sys.argv.(4) else None in

  (* Tell the header-stripping transforms which file is the "real" source *)
  Slicer_transforms.Remove_header_globals.original_source := original_source;

  (* Initialize CIL *)
  initCIL ();

  (* Step 1: Parse preprocessed C into CIL AST *)
  Printf.eprintf "[slicer] Parsing %s ...\n%!" input_file;
  let cil_file = Frontc.parse input_file () in

  (* Collect pre-transform stats *)
  let before_stats = Slicer_transforms.Stats.collect cil_file in
  Printf.eprintf "[slicer] Before: %s\n%!" (Slicer_transforms.Stats.to_string before_stats);

  (* Step 2: Apply transformation pipeline *)
  Slicer_transforms.Transforms.apply_all cil_file;

  (* Collect post-transform stats *)
  let after_stats = Slicer_transforms.Stats.collect cil_file in
  Printf.eprintf "[slicer] After:  %s\n%!" (Slicer_transforms.Stats.to_string after_stats);

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
  (match stats_file with
   | Some path ->
     let sc = open_out path in
     Printf.fprintf sc "before_semicolon_loc=%d\n" before_stats.semicolon_loc;
     Printf.fprintf sc "after_semicolon_loc=%d\n" after_stats.semicolon_loc;
     close_out sc;
     Printf.eprintf "[slicer] Stats written to %s\n%!" path
   | None -> ());

  Printf.eprintf "[slicer] Done.\n%!"
