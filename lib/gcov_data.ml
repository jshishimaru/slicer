(* gcov_data.ml — Parse gcov --json-format output for dead-code info
 *
 * gcov produces JSON with per-file data containing:
 *   - functions: name, execution_count, start_line, end_line
 *   - lines: line_number, count, function_name
 *
 * We extract:
 *   1. Dead functions (execution_count = 0)
 *   2. Dead lines (count = 0)
 *
 * The JSON is produced by: gcov --json-format --stdout <source.c>
 * and piped through gunzip (gcov emits gzipped JSON by default with -j).
 *)

(** Parsed gcov coverage data. *)
type t = {
  dead_functions : (string, bool) Hashtbl.t;
  (** Function names with execution_count = 0 *)

  dead_lines : (int, bool) Hashtbl.t;
  (** Line numbers with count = 0 *)

  executed_lines : (int, bool) Hashtbl.t;
  (** Line numbers with count > 0 — used to confirm a line was instrumented *)
}

(** Empty gcov data — used as a no-op when no gcov JSON is available. *)
let empty () : t = {
  dead_functions = Hashtbl.create 0;
  dead_lines = Hashtbl.create 0;
  executed_lines = Hashtbl.create 0;
}

(** Is this function proven dead (never called) by gcov? *)
let is_dead_function (data : t) (name : string) : bool =
  Hashtbl.mem data.dead_functions name

(** Is this line proven dead (never executed) by gcov? *)
let is_dead_line (data : t) (line : int) : bool =
  Hashtbl.mem data.dead_lines line

(** Is this line confirmed dead with surrounding context?
    Returns true only if the line AND its neighbors (±2) are all dead
    or uninstrumented — no executed line within the window.
    This guards against CIL line-number shifts causing false removals. *)
let is_safely_dead_line (data : t) (line : int) : bool =
  if not (Hashtbl.mem data.dead_lines line) then false
  else
    (* Check that no neighboring line (±2) was actually executed *)
    let safe = ref true in
    for l = line - 2 to line + 2 do
      if Hashtbl.mem data.executed_lines l then safe := false
    done;
    !safe

(** Was this line instrumented at all by gcov? *)
let is_instrumented (data : t) (line : int) : bool =
  Hashtbl.mem data.dead_lines line || Hashtbl.mem data.executed_lines line

(** Parse a single file entry from the gcov JSON. *)
let parse_file_entry (data : t) (file_json : Yojson.Basic.t) : unit =
  let open Yojson.Basic.Util in
  (* Parse functions *)
  let functions = file_json |> member "functions" |> to_list in
  List.iter (fun func ->
    let name = func |> member "name" |> to_string in
    let exec_count = func |> member "execution_count" |> to_int in
    if exec_count = 0 then
      Hashtbl.replace data.dead_functions name true
  ) functions;
  (* Parse lines *)
  let lines = file_json |> member "lines" |> to_list in
  List.iter (fun line ->
    let line_number = line |> member "line_number" |> to_int in
    let count = line |> member "count" |> to_int in
    if count = 0 then
      Hashtbl.replace data.dead_lines line_number true
    else
      Hashtbl.replace data.executed_lines line_number true
  ) lines

(** Load gcov JSON from a file path.
    The JSON structure is:
    { "files": [ { "file": "...", "functions": [...], "lines": [...] }, ... ] }
*)
let load (path : string) : t =
  let data = {
    dead_functions = Hashtbl.create 64;
    dead_lines = Hashtbl.create 256;
    executed_lines = Hashtbl.create 256;
  } in
  (try
    let json = Yojson.Basic.from_file path in
    let open Yojson.Basic.Util in
    let files = json |> member "files" |> to_list in
    List.iter (parse_file_entry data) files;
    Printf.eprintf "[gcov_data] Loaded: %d dead functions, %d dead lines, %d executed lines\n%!"
      (Hashtbl.length data.dead_functions)
      (Hashtbl.length data.dead_lines)
      (Hashtbl.length data.executed_lines)
  with
  | Yojson.Json_error msg ->
    Printf.eprintf "[gcov_data] WARNING: JSON parse error: %s\n%!" msg
  | exn ->
    Printf.eprintf "[gcov_data] WARNING: Could not load %s: %s\n%!" path
      (Printexc.to_string exn));
  data
