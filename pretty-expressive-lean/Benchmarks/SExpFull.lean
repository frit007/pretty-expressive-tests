import BenchTool
import BaseFormatter

open PrettyFormat

-- Define the S-expression type
inductive SExpr where
  | atom : String → SExpr
  | list : List SExpr → SExpr
deriving Repr, Inhabited

open SExpr

-- Generate a balanced S-expression tree
partial def testExpr : Nat → Nat → (SExpr × Nat)
  | 0, c => (atom (toString c), c + 1)
  | n + 1, c =>
    let (t1, c1) := testExpr n c
    let (t2, c2) := testExpr n c1
    (list [t1, t2], c2)

def hsep (xs : Array Doc) : Doc := combine (fun l r => ((l) <> " " <> r)) xs
def vcat (xs : Array Doc) : Doc := combine (fun l r => l <>Doc.hardNl<> r) xs
-- let hsep  = fold_doc (fun x y -> x <+> space <+> y)
def mysep (xs : Array Doc) : Doc := hsep xs <^> vcat xs

-- Pretty-print an S-expression as a FormatM Doc
partial def pp : SExpr → FormatM Doc
  | atom s => return toDoc s
  | list xs => do
    let docs ← xs.mapM pp
    fmt (toDoc "(" <+> ((mysep docs.toArray) <+> toDoc ")"))

-- Benchmark runner in FormatM style
def sexprBenchmark (c : Config) : IO Info := do
  let (sexpr, _) := testExpr c.size 0
  let (doc, state) := simpleFormattingContext (pp sexpr)

  let out ← doc.prettyPrint DefaultCost (cacheSize := state.nextId) (col := 0) (widthLimit := c.pageWidth) (computationWidth := c.computationWidth)

  return {
    out := out,
    cost := "0",
    isTainted := false
  }

-- Main entry point
def main (originalArgs : List String) : IO Unit := do
  let cfg ← parseArgs originalArgs
  runBenchmark "sexp-full" cfg sexprBenchmark
