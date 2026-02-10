(* transform.ml — Common type for all CIL transformations *)

open GoblintCil

(** A transformation is a named function that mutates a CIL file in-place. *)
type t = {
  name : string;            (** Human-readable name (for logging) *)
  apply : file -> unit;     (** Mutate the CIL AST in place *)
}
