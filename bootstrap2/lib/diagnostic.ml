(* One error type for every pass, so the sequence of passes can be written once
   and each caller differs only in how it renders what stopped. Each pass keeps
   its own error record; this is what they are widened to at the boundary. *)

type stage =
  | Scan
  | Parse
  | Load
  | Meta
  | Desugar
  | Value_mono
  | Type
  | Type_mono
  | Resolve
  | Reflect
  | Cps
  | Verify
  | Runtime

type error =
  { stage : stage
  ; span : Ast.span
  ; message : string
  }

let stage_name = function
  | Scan -> "Scan"
  | Parse -> "Parse"
  | Load -> "Load"
  | Meta -> "Meta"
  | Desugar -> "Desugar"
  | Value_mono -> "Value monomorphize"
  | Type -> "Type"
  | Type_mono -> "Type monomorphize"
  | Resolve -> "Resolve"
  | Reflect -> "Reflect"
  | Cps -> "CPS"
  | Verify -> "Verify"
  | Runtime -> "Runtime"

(* A program the compiler rejected is the user's fault; one that got past the
   checker and then broke is the compiler's. *)
let exit_code = function
  | Scan | Parse | Load | Meta | Desugar | Value_mono | Type | Type_mono | Resolve | Reflect -> 65
  | Cps | Verify | Runtime -> 70

let at stage span message = { stage; span; message }
let one stage span message = Error [ { stage; span; message } ]
