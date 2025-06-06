import BenchTool
import BaseFormatter
import Lean.Data.Json

open Lean
open PrettyFormat

def jsonFile : Nat → String
  | 1 => "/1k.json"
  | 2 => "/10k.json"
  | x => panic! s!"size {x} does not correspond to a JSON file"

def enclose_sep (left right sep : Doc) (ds : Array Doc) : FormatM Doc :=
  match ds with
  | #[] => return left <+> right
  | #[d] => return left <+> d <+> right
  | ds =>
    let vcat := (combine (.<>Doc.hardNl<>sep<>.) ds)
    -- so we do not leave any space after the comma?
    let hcat := (combine (fun l r => (flattenDoc l)<+>sep<+>r) ds)
    -- return left <+> (vcat <^> hcat) <+> right
    return alignDoc (left <> (vcat <^> hcat) <> right)

partial def pp : Json → FormatM Doc
  | .null => return toDoc "null"
  | .bool b => return toDoc (if b then "true" else "false")
  | .num (n : JsonNumber) => return toDoc s!"{n}.0"
  | .str (s : String) => return toDoc s!"\"{s}\""
  | .arr (elems : Array Json) => do
    let xs ← elems.mapM pp
    fmt (← enclose_sep (toDoc "[") (toDoc "]") (toDoc ",") xs)

  -- uses RBNode instead of RBMap because RBMap is a def
  -- and thus currently cannot be used to define a type that
  -- is recursive in one of its parameters
  | .obj (kvPairs : RBNode String (fun _ => Json)) => do
    -- Convert the RBNode to a list and sort by key
    let sorted := (kvPairs.toArray.qsort (fun a b => a.1 < b.1))
    -- Pretty-print each key-value pair
    let xs ← sorted.mapM fun e => do
      let vk ← pp e.snd
      return toDoc s!"\"{e.fst}\": " <+> vk
    -- Enclose with braces and separate with commas
    fmt (← enclose_sep (toDoc "{") (toDoc "}") (toDoc ",") xs)


-- (* NOTE: Bernardy's paper formats JSON in the Haskell style, *)
-- (* which is unconventional. We follow the style, however, *)
-- (* to obtain comparable data points. *)

def jsonBenchmark (c : Config):  IO Info := do
  let path := match (← IO.getEnv "BENCHDATA") with
  | some s => s
  | _ => "../data"
  let path := path ++ (jsonFile c.size)
  let content ← IO.FS.readFile (path)

  -- let allLines := content.splitOn "\n"
  -- let lines := allLines.take c.size -- unfortunate that we pay for reading the entire file
  let (doc, state) := match Json.parse content with
  | Except.ok json =>
    simpleFormattingContext (pp json)
  | Except.error _ => simpleFormattingContext (return "no" <_> "json")

  IO.println s!"pageWidth: {c.pageWidth} computationWidth {c.computationWidth}"
  let a ← doc.prettyPrint DefaultCost (cacheSize := state.nextId) (col := 0) (widthLimit := c.pageWidth) (computationWidth := c.computationWidth)
  return {
    out := a,
    cost := "0",
    isTainted := false
  }

-- Main entry point
def main (originalArgs : List String): IO Unit := do
  let cfg ← parseArgs originalArgs
  runBenchmark "json" cfg jsonBenchmark
