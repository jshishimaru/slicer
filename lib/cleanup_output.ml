(* cleanup_output.ml — Post-processing cleanup for CIL output *)
(* CIL's defaultCilPrinter produces valid but verbose C:
   - Double braces: outer { from pFunDecl, inner { from pBlock
   - Extra blank lines between declarations and body
   - Excessive whitespace in declarations (e.g., "int a  =    10;")

   This module cleans up the emitted C text after dumpFile. *)

let cleanup (text : string) : string =
  let lines = String.split_on_char '\n' text in

  (* Pass 1: collapse runs of blank lines into at most one *)
  let collapse_blanks lines =
    let rec aux prev_blank = function
      | [] -> []
      | line :: rest ->
        let is_blank = String.trim line = "" in
        if is_blank && prev_blank then
          aux true rest
        else
          line :: aux is_blank rest
    in
    aux false lines
  in

  (* Pass 2: collapse multiple spaces into single space,
     but preserve leading indentation and string literals *)
  let collapse_spaces line =
    let trimmed = String.trim line in
    if trimmed = "" then line
    else
      (* Find leading whitespace *)
      let len = String.length line in
      let indent_end = ref 0 in
      while !indent_end < len && (line.[!indent_end] = ' ' || line.[!indent_end] = '\t') do
        incr indent_end
      done;
      let indent = String.sub line 0 !indent_end in
      let rest = String.sub line !indent_end (len - !indent_end) in
      (* Collapse spaces in rest, respecting string literals *)
      let buf = Buffer.create (String.length rest) in
      let in_string = ref false in
      let in_char = ref false in
      let prev_space = ref false in
      let prev_ch = ref '\x00' in
      String.iter (fun c ->
        if !in_string then begin
          Buffer.add_char buf c;
          if c = '"' && !prev_ch <> '\\' then in_string := false;
          prev_space := false
        end else if !in_char then begin
          Buffer.add_char buf c;
          if c = '\'' && !prev_ch <> '\\' then in_char := false;
          prev_space := false
        end else if c = '"' then begin
          in_string := true;
          prev_space := false;
          Buffer.add_char buf c
        end else if c = '\'' then begin
          in_char := true;
          prev_space := false;
          Buffer.add_char buf c
        end else if c = ' ' then begin
          if not !prev_space then Buffer.add_char buf ' ';
          prev_space := true
        end else begin
          prev_space := false;
          Buffer.add_char buf c
        end;
        prev_ch := c
      ) rest;
      indent ^ Buffer.contents buf
  in

  (* Pass 3: remove redundant inner braces from function bodies.
     CIL's pFunDecl emits outer { }, and pBlock adds inner { } for sbody.
     The output pattern for each function is:
       func_sig(...)
       {                   ← outer open (standalone line)
         <local decls>
         {                 ← inner open (standalone line, indented)
         <body stmts>
       }                   ← inner close (standalone, col 0)
       }                   ← outer close (standalone, col 0)

     We detect standalone "}" on consecutive lines (inner close + outer close)
     and then find the matching inner open "{" by counting braces properly
     within each line, only matching standalone "{" lines. *)
  let remove_inner_braces lines =
    let arr = Array.of_list lines in
    let n = Array.length arr in
    let skip = Array.make n false in

    (* Count net braces in a line (opening minus closing), excluding string literals *)
    let count_braces_in_line line =
      let opens = ref 0 in
      let closes = ref 0 in
      let in_str = ref false in
      let in_chr = ref false in
      let prev = ref '\x00' in
      String.iter (fun c ->
        if !in_str then
          (if c = '"' && !prev <> '\\' then in_str := false)
        else if !in_chr then
          (if c = '\'' && !prev <> '\\' then in_chr := false)
        else if c = '"' then in_str := true
        else if c = '\'' then in_chr := true
        else if c = '{' then incr opens
        else if c = '}' then incr closes;
        prev := c
      ) line;
      (!opens, !closes)
    in

    (* Find consecutive standalone "}" lines *)
    for i = 0 to n - 2 do
      if not skip.(i) && not skip.(i + 1) then begin
        let t1 = String.trim arr.(i) in
        let t2 = String.trim arr.(i + 1) in
        if t1 = "}" && t2 = "}" then begin
          (* i is inner close, i+1 is outer close.
             Scan backwards from i to find the matching inner open.
             Start with depth=1 because we need to match one open brace. *)
          let depth = ref 1 in
          let j = ref (i - 1) in
          let found = ref false in
          while !j >= 0 && not !found do
            let (opens, closes) = count_braces_in_line arr.(!j) in
            let trimmed = String.trim arr.(!j) in
            (* Going backwards: closes need more matching, opens satisfy matches *)
            depth := !depth + closes - opens;
            (* If this line is a standalone "{" and depth reached 0, it's our match *)
            if trimmed = "{" && !depth = 0 then begin
              skip.(!j) <- true;
              skip.(i) <- true;
              found := true
            end;
            decr j
          done
        end
      end
    done;
    let result = ref [] in
    for i = n - 1 downto 0 do
      if not skip.(i) then result := arr.(i) :: !result
    done;
    !result
  in

  (* Pass 4: remove blank lines right after opening brace *)
  let remove_blank_after_brace lines =
    let rec aux = function
      | [] -> []
      | line :: rest ->
        let trimmed = String.trim line in
        if String.length trimmed > 0 && trimmed.[String.length trimmed - 1] = '{' then
          (* Consume following blank lines *)
          let rec skip_blanks = function
            | [] -> [line]
            | next :: rest2 ->
              if String.trim next = "" then skip_blanks rest2
              else line :: aux (next :: rest2)
          in
          skip_blanks rest
        else
          line :: aux rest
    in
    aux lines
  in

  (* Pass 5: convert CIL's while-loops back to for-loops where possible.
     CIL transforms "for (init; cond; incr) body" into:
       init;
       while (cond) {
         body
         incr;
       }
     We detect this pattern textually and reconstruct the for loop.
     Requirements:
       - Line before "while (...) {" is a simple statement (the init)
       - Last statement inside the while body (line before closing "}") is
         a simple expression statement (the increment)
       - Both init and incr are single-line simple statements *)
  let while_to_for lines =
    let arr = Array.of_list lines in
    let n = Array.length arr in
    let skip = Array.make n false in       (* lines to delete *)
    let replace = Hashtbl.create 16 in     (* line index → replacement text *)

    (* Check if a trimmed line looks like a simple statement (assignment, inc, dec).
       Must end with ';' and not be a control-flow keyword or a label. *)
    let is_simple_stmt trimmed =
      String.length trimmed > 0
      && trimmed.[String.length trimmed - 1] = ';'
      && (let first_word =
            try String.sub trimmed 0 (String.index trimmed ' ')
            with Not_found -> trimmed
          in
          (* Reject CIL labels like "__Cont:" or "while_break:" that end with ':' *)
          let is_label =
            String.length first_word > 0
            && first_word.[String.length first_word - 1] = ':'
          in
          (* Also reject lines containing a label anywhere: "ident: ... ;" *)
          let has_colon_before_eq =
            try
              let colon_pos = String.index trimmed ':' in
              (* Make sure the colon isn't inside a ternary or after '=' *)
              let eq_pos =
                try String.index trimmed '='
                with Not_found -> String.length trimmed
              in
              colon_pos < eq_pos
            with Not_found -> false
          in
          (not is_label) && (not has_colon_before_eq)
          && not (List.mem first_word
                 ["if"; "else"; "while"; "for"; "switch"; "return";
                  "break"; "continue"; "goto"; "case"; "default";
                  "do"; "int"; "char"; "short"; "long"; "unsigned";
                  "signed"; "float"; "double"; "void"; "struct";
                  "union"; "enum"; "const"; "volatile"; "static";
                  "extern"; "register"; "auto"; "typedef";
                  "uint8_t"; "uint16_t"; "uint32_t"; "uint64_t";
                  "int8_t"; "int16_t"; "int32_t"; "int64_t";
                  "size_t"; "ssize_t"; "ptrdiff_t"; "bool";
                  "uint_fast8_t"; "uint_fast16_t"; "uint_fast32_t";
                  "uint_fast64_t"; "int_fast8_t"; "int_fast16_t";
                  "int_fast32_t"; "int_fast64_t";
                  "uint_least8_t"; "uint_least16_t"; "uint_least32_t";
                  "uint_least64_t"; "int_least8_t"; "int_least16_t";
                  "int_least32_t"; "int_least64_t"]))
    in

    (* Find the closing brace of a while block starting at line i (the while line).
       Returns the index of the "}" line, or -1 if not found. *)
    let find_closing_brace start =
      let depth = ref 0 in
      let result = ref (-1) in
      let j = ref start in
      while !j < n && !result = (-1) do
        let line = arr.(!j) in
        let in_str = ref false in
        let in_chr = ref false in
        let prev = ref '\x00' in
        String.iter (fun c ->
          if !in_str then
            (if c = '"' && !prev <> '\\' then in_str := false)
          else if !in_chr then
            (if c = '\'' && !prev <> '\\' then in_chr := false)
          else if c = '"' then in_str := true
          else if c = '\'' then in_chr := true
          else if c = '{' then incr depth
          else if c = '}' then begin
            decr depth;
            if !depth = 0 then result := !j
          end;
          prev := c
        ) line;
        incr j
      done;
      !result
    in

    for i = 0 to n - 1 do
      let trimmed = String.trim arr.(i) in
      (* Match "while (<cond>) {" *)
      if String.length trimmed > 7
         && String.sub trimmed 0 6 = "while "
         && trimmed.[String.length trimmed - 1] = '{'
      then begin
        (* Check the line before: must be a simple init statement *)
        let has_init = i > 0 && not skip.(i - 1) in
        if has_init then begin
          let init_trimmed = String.trim arr.(i - 1) in
          if is_simple_stmt init_trimmed then begin
            (* Find closing brace *)
            let close_idx = find_closing_brace i in
            if close_idx > i + 1 then begin
              (* The increment is the last statement before the closing brace.
                 Find it: scan backwards from close_idx - 1 skipping blank lines *)
              let incr_idx = ref (close_idx - 1) in
              while !incr_idx > i && String.trim arr.(!incr_idx) = "" do
                decr incr_idx
              done;
              let incr_trimmed = String.trim arr.(!incr_idx) in
              if !incr_idx > i && is_simple_stmt incr_trimmed then begin
                (* Extract the condition from "while (<cond>) {" *)
                let while_content = String.trim trimmed in
                (* Find the opening paren after "while" *)
                let paren_start =
                  try String.index while_content '(' with Not_found -> -1
                in
                (* Find the last ')' before '{' *)
                let paren_end = ref (-1) in
                for k = String.length while_content - 1 downto 0 do
                  if while_content.[k] = ')' && !paren_end = (-1) then
                    paren_end := k
                done;
                if paren_start >= 0 && !paren_end > paren_start then begin
                  let cond = String.sub while_content (paren_start + 1)
                      (!paren_end - paren_start - 1) in
                  (* Strip trailing ';' from init and incr *)
                  let strip_semi s =
                    let s = String.trim s in
                    if String.length s > 0 && s.[String.length s - 1] = ';' then
                      String.trim (String.sub s 0 (String.length s - 1))
                    else s
                  in
                  let init_expr = strip_semi init_trimmed in
                  let incr_expr = strip_semi incr_trimmed in
                  (* Get the indentation of the while line *)
                  let indent =
                    let len = String.length arr.(i) in
                    let k = ref 0 in
                    while !k < len && (arr.(i).[!k] = ' ' || arr.(i).[!k] = '\t') do
                      incr k
                    done;
                    String.sub arr.(i) 0 !k
                  in
                  (* Build the for loop *)
                  let for_line = Printf.sprintf "%sfor (%s; %s; %s) {"
                      indent init_expr cond incr_expr in
                  (* Mark init line and incr line for removal *)
                  skip.(i - 1) <- true;
                  skip.(!incr_idx) <- true;
                  (* Replace the while line with the for line *)
                  Hashtbl.replace replace i for_line
                end
              end
            end
          end
        end
      end
    done;
    let result = ref [] in
    for i = n - 1 downto 0 do
      if not skip.(i) then begin
        match Hashtbl.find_opt replace i with
        | Some line -> result := line :: !result
        | None -> result := arr.(i) :: !result
      end
    done;
    !result
  in

  (* Pass 6: collapse multi-line initializers onto single lines.
     CIL splits array/struct initializers across many lines with deep
     indentation.  We detect initializer blocks (line containing '='
     followed by continuation lines until we see ';') and join them
     into a single line, then trim excessive internal whitespace. *)
  let collapse_initializers lines =
    let arr = Array.of_list lines in
    let n = Array.length arr in
    let result = Buffer.create (n * 40) in
    let i = ref 0 in
    while !i < n do
      let line = arr.(!i) in
      let trimmed = String.trim line in
      (* Detect start of a multi-line initializer:
         - line does NOT end with ';' (it continues)
         - line contains '=' (it's a declaration with initializer)
         - or line ends with ',' or '{' and is continuation of one *)
      let ends_with_semi =
        String.length trimmed > 0 && trimmed.[String.length trimmed - 1] = ';'
      in
      let has_equals =
        try let _ = String.index trimmed '=' in true
        with Not_found -> false
      in
      if (not ends_with_semi) && has_equals
         && String.length trimmed > 0 then begin
        (* Start collecting: join lines until we find one ending with ';' *)
        let buf = Buffer.create 200 in
        Buffer.add_string buf line;
        incr i;
        let found_end = ref false in
        while !i < n && not !found_end do
          let next = arr.(!i) in
          let next_trimmed = String.trim next in
          (* Join with a single space *)
          Buffer.add_char buf ' ';
          Buffer.add_string buf next_trimmed;
          if String.length next_trimmed > 0
             && next_trimmed.[String.length next_trimmed - 1] = ';' then
            found_end := true;
          incr i
        done;
        (* Collapse internal runs of whitespace in the joined line *)
        let joined = Buffer.contents buf in
        let collapsed = Buffer.create (String.length joined) in
        let leading_end = ref 0 in
        let len = String.length joined in
        while !leading_end < len
              && (joined.[!leading_end] = ' ' || joined.[!leading_end] = '\t') do
          incr leading_end
        done;
        Buffer.add_string collapsed (String.sub joined 0 !leading_end);
        let prev_space = ref false in
        let in_str = ref false in
        let in_chr = ref false in
        let prev_ch = ref '\x00' in
        for k = !leading_end to len - 1 do
          let c = joined.[k] in
          if !in_str then begin
            Buffer.add_char collapsed c;
            if c = '"' && !prev_ch <> '\\' then in_str := false;
            prev_space := false
          end else if !in_chr then begin
            Buffer.add_char collapsed c;
            if c = '\'' && !prev_ch <> '\\' then in_chr := false;
            prev_space := false
          end else if c = '"' then begin
            in_str := true;
            prev_space := false;
            Buffer.add_char collapsed c
          end else if c = '\'' then begin
            in_chr := true;
            prev_space := false;
            Buffer.add_char collapsed c
          end else if c = ' ' || c = '\t' then begin
            if not !prev_space then Buffer.add_char collapsed ' ';
            prev_space := true
          end else begin
            prev_space := false;
            Buffer.add_char collapsed c
          end;
          prev_ch := c
        done;
        Buffer.add_string result (Buffer.contents collapsed);
        Buffer.add_char result '\n'
      end else begin
        Buffer.add_string result line;
        Buffer.add_char result '\n';
        incr i
      end
    done;
    let s = Buffer.contents result in
    String.split_on_char '\n' s
  in

  let lines = collapse_blanks lines in
  let lines = remove_inner_braces lines in
  let lines = remove_blank_after_brace lines in
  let lines = List.map collapse_spaces lines in
  let lines = while_to_for lines in
  let lines = collapse_initializers lines in
  (* Final: collapse blanks again after transformations *)
  let lines = collapse_blanks lines in
  String.concat "\n" lines
