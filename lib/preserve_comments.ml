(* preserve_comments.ml — Re-inject comments from the original source file
 *
 * CIL's parser strips all comments from the C source.  This transform reads
 * the original (pre-preprocessed) source, extracts block comments and
 * single-line comments, associates each with the code entity that follows it,
 * and injects them back into the CIL AST as GText nodes.
 *
 * Comment categories:
 *   1. Top-of-file   — comments before any code line → prepended
 *   2. Before-entity — comments immediately before a function/global →
 *                      matched by name and placed before the corresponding global
 *   3. End-of-file   — comments after all code (e.g. statistics) → appended
 *
 * Uses the same original_source ref as add_includes.ml and
 * remove_header_globals.ml.
 *)

open GoblintCil

(* ── Helpers ───────────────────────────────────────────────── *)

(** Check if a line contains the end of a block comment ( * / ) *)
let has_block_comment_end (line : string) : bool =
  let len = String.length line in
  if len < 2 then false
  else
    let found = ref false in
    for k = 0 to len - 2 do
      if line.[k] = '*' && line.[k + 1] = '/' then
        found := true
    done;
    !found

(** Try to extract the first C identifier from a code line, skipping
    common type / qualifier keywords.  Used to match comments to globals. *)
let extract_name_from_line (line : string) : string option =
  let trimmed = String.trim line in
  if String.length trimmed = 0 then None
  else begin
    let buf = Buffer.create 64 in
    let tokens = ref [] in
    String.iter (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9') || c = '_' then
        Buffer.add_char buf c
      else begin
        let tok = Buffer.contents buf in
        if tok <> "" then tokens := tok :: !tokens;
        Buffer.clear buf
      end
    ) trimmed;
    let tok = Buffer.contents buf in
    if tok <> "" then tokens := tok :: !tokens;
    let tokens = List.rev !tokens in
    let skip = [
      "static"; "extern"; "const"; "volatile"; "inline"; "register";
      "signed"; "unsigned"; "void"; "char"; "short"; "int"; "long";
      "float"; "double"; "struct"; "union"; "enum"; "typedef";
      "int8_t"; "int16_t"; "int32_t"; "int64_t";
      "uint8_t"; "uint16_t"; "uint32_t"; "uint64_t";
      "size_t"; "ssize_t"; "ptrdiff_t"; "bool";
      "uint_fast8_t"; "uint_fast16_t"; "uint_fast32_t"; "uint_fast64_t";
      "int_fast8_t"; "int_fast16_t"; "int_fast32_t"; "int_fast64_t";
    ] in
    let rec find = function
      | [] -> None
      | t :: rest ->
        if List.mem t skip then find rest
        else Some t
    in
    find tokens
  end

(* ── Comment extraction ────────────────────────────────────── *)

(** A comment block is one or more contiguous comment lines together with
    the "anchor" — the first non-blank, non-comment line that follows it. *)
type comment_block = {
  text : string;       (** The raw comment text (may be multi-line) *)
  anchor : string;     (** First non-blank, non-comment line after this comment,
                           or "" if the comment is at the very end of the file *)
}

(** Read the original source file and extract all comment blocks together
    with their anchors (the next non-comment, non-blank line). *)
let extract_comments (path : string) : comment_block list =
  let ic = open_in path in
  let lines = ref [] in
  (try while true do
     lines := input_line ic :: !lines
   done with End_of_file -> ());
  close_in ic;
  let lines = Array.of_list (List.rev !lines) in
  let n = Array.length lines in

  let results = ref [] in
  let i = ref 0 in
  let in_block_comment = ref false in

  while !i < n do
    let line = lines.(!i) in
    let trimmed = String.trim line in

    if !in_block_comment then begin
      (* We are continuing inside a multi-line block comment — this shouldn't
         happen at the top of the loop because we consume entire block comments
         below.  But handle it defensively. *)
      incr i
    end
    (* Detect start of a block comment *)
    else if String.length trimmed >= 2 && String.sub trimmed 0 2 = "/*" then begin
      let buf = Buffer.create 256 in
      (* Consume the block comment (may span multiple lines) *)
      in_block_comment := true;
      while !i < n && !in_block_comment do
        let l = lines.(!i) in
        Buffer.add_string buf l;
        Buffer.add_char buf '\n';
        if has_block_comment_end l then
          in_block_comment := false;
        incr i
      done;
      (* After a block comment, skip blank lines and collect any immediately
         following comments into the same block *)
      let continue = ref true in
      while !i < n && !continue do
        let t = String.trim lines.(!i) in
        if t = "" then begin
          Buffer.add_string buf lines.(!i);
          Buffer.add_char buf '\n';
          incr i
        end else if String.length t >= 2 && String.sub t 0 2 = "/*" then begin
          (* Another block comment — merge into the same comment block *)
          in_block_comment := true;
          while !i < n && !in_block_comment do
            let l = lines.(!i) in
            Buffer.add_string buf l;
            Buffer.add_char buf '\n';
            if has_block_comment_end l then
              in_block_comment := false;
            incr i
          done
        end else if String.length t >= 2 && String.sub t 0 2 = "//" then begin
          Buffer.add_string buf lines.(!i);
          Buffer.add_char buf '\n';
          incr i
        end else
          continue := false
      done;
      let anchor = if !i < n then String.trim lines.(!i) else "" in
      results := { text = Buffer.contents buf; anchor } :: !results
    end
    (* Detect single-line comments *)
    else if String.length trimmed >= 2 && String.sub trimmed 0 2 = "//" then begin
      let buf = Buffer.create 256 in
      while !i < n &&
            (let t = String.trim lines.(!i) in
             (String.length t >= 2 && String.sub t 0 2 = "//")
             || t = "") do
        Buffer.add_string buf lines.(!i);
        Buffer.add_char buf '\n';
        incr i
      done;
      let anchor = if !i < n then String.trim lines.(!i) else "" in
      results := { text = Buffer.contents buf; anchor } :: !results
    end
    else
      incr i
  done;
  List.rev !results

(* ── Injection into CIL AST ───────────────────────────────── *)

(** Try to get the name of a CIL global *)
let global_name (g : global) : string option =
  match g with
  | GFun (fd, _) -> Some fd.svar.vname
  | GVar (vi, _, _) -> Some vi.vname
  | GVarDecl (vi, _) -> Some vi.vname
  | GType (ti, _) -> Some ti.tname
  | GCompTag (ci, _) -> Some ci.cname
  | GCompTagDecl (ci, _) -> Some ci.cname
  | GEnumTag (ei, _) -> Some ei.ename
  | GEnumTagDecl (ei, _) -> Some ei.ename
  | _ -> None

let apply (f : file) : unit =
  match !(Remove_header_globals.original_source) with
  | None ->
    Printf.eprintf "[preserve_comments] No original source — skipping\n%!"
  | Some src_path ->
    let comments = extract_comments src_path in
    if comments = [] then
      Printf.eprintf "[preserve_comments] No comments found in %s\n%!" src_path
    else begin
      (* Partition comments into: top-of-file, before-entity, end-of-file *)
      let top_comments = ref [] in
      let end_comments = ref [] in
      (* Map from entity name → list of comment texts to inject before it *)
      let entity_comments : (string, string list) Hashtbl.t = Hashtbl.create 16 in

      List.iter (fun (cb : comment_block) ->
        if cb.anchor = "" then
          (* No code follows — end-of-file comment (e.g. statistics block) *)
          end_comments := cb.text :: !end_comments
        else if String.length cb.anchor > 0 && cb.anchor.[0] = '#' then
          (* Anchor is a preprocessor directive — treat as top-of-file *)
          top_comments := cb.text :: !top_comments
        else begin
          match extract_name_from_line cb.anchor with
          | Some name ->
            let existing =
              match Hashtbl.find_opt entity_comments name with
              | Some prev -> prev @ [cb.text]
              | None -> [cb.text]
            in
            Hashtbl.replace entity_comments name existing
          | None ->
            (* Can't determine anchor name — treat as top-of-file *)
            top_comments := cb.text :: !top_comments
        end
      ) comments;

      (* Build new globals list with comments injected *)
      let new_globals = ref [] in
      let injected_count = ref 0 in

      (* Prepend top-of-file comments (in original order) *)
      List.iter (fun text ->
        new_globals := GText text :: !new_globals;
        incr injected_count
      ) (List.rev !top_comments);

      (* Walk existing globals: inject matching comments before each *)
      List.iter (fun g ->
        (match global_name g with
         | Some name ->
           (match Hashtbl.find_opt entity_comments name with
            | Some texts ->
              List.iter (fun text ->
                new_globals := GText text :: !new_globals;
                incr injected_count
              ) texts;
              Hashtbl.remove entity_comments name
            | None -> ())
         | None -> ());
        new_globals := g :: !new_globals
      ) f.globals;

      (* Append end-of-file comments (in original order) *)
      List.iter (fun text ->
        new_globals := GText text :: !new_globals;
        incr injected_count
      ) (List.rev !end_comments);

      f.globals <- List.rev !new_globals;

      Printf.eprintf "[preserve_comments] Injected %d comment block(s) from %s\n%!"
        !injected_count src_path
    end

let transform : Transform.t = {
  name = "Preserve comments";
  apply;
}
