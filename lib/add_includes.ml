(* add_includes.ml — Re-add #include directives from the original source *)
(* After remove_header_globals strips all header-expanded globals, the
   output would fail to compile because type definitions, macros, etc.
   are gone.  This transform scans the *original* source file for
   #include lines and prepends them as GText globals so that the
   emitted C re-includes the necessary headers. *)

open GoblintCil

(** Extract #include lines from a file, preserving order. *)
let extract_includes (path : string) : string list =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line ->
      let trimmed = String.trim line in
      if String.length trimmed > 8 &&
         String.sub trimmed 0 8 = "#include" then
        loop (line :: acc)
      else
        loop acc
    | exception End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

let apply (f : file) : unit =
  match !(Remove_header_globals.original_source) with
  | None ->
    Printf.eprintf "[slicer]   (no original source path — skipping #include insertion)\n%!"
  | Some src_path ->
    let includes = extract_includes src_path in
    if includes = [] then
      Printf.eprintf "[slicer]   No #include lines found in %s\n%!" src_path
    else begin
      let include_globals =
        List.map (fun line -> GText (line ^ "\n")) includes
      in
      (* Prepend the #include lines before all other globals *)
      f.globals <- include_globals @ f.globals;
      Printf.eprintf "[slicer]   Added %d #include directive(s)\n%!" (List.length includes)
    end

let transform : Transform.t = {
  name = "Add #include directives";
  apply;
}
