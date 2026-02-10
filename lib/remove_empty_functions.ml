(* remove_empty_functions.ml — Remove functions with empty bodies *)
(* A CIL visitor that removes GFun globals whose body has zero
   statements. Never removes main. Combined with RmUnused, this
   effectively strips dead helper stubs. *)

open GoblintCil

class visitor = object
  inherit nopCilVisitor
  method! vglob g =
    match g with
    | GFun (fd, _) ->
      if fd.sbody.bstmts = [] && fd.svar.vname <> "main" then
        ChangeTo []
      else
        DoChildren
    | _ -> DoChildren
end

let apply (f : file) : unit =
  visitCilFile (new visitor) f

let transform : Transform.t = {
  name = "Remove empty functions";
  apply;
}
