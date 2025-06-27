import BenchTool
import BaseFormatter
import Lean.Data.Json

open PrettyFormat
open Lean

-- Define the S-expression type
inductive SExpr where
  | atom : String → SExpr
  | list : Array SExpr → SExpr
deriving Repr, Inhabited

open SExpr

-- Generate a balanced S-expression tree
-- partial def testExpr : Nat → Nat → (SExpr × Nat)
--   | 0, c => (atom (toString c), c + 1)
--   | n + 1, c =>
--     let (t1, c1) := testExpr n c
--     let (t2, c2) := testExpr n c1
--     (list [t1, t2], c2)

def foldDoc (f : Doc → Doc → Doc) (ds : List Doc) :=
  match ds with
  | [] => toDoc ""
  | x :: xs => xs.foldl f x

-- def hsep (xs : Array Doc) : Doc := foldDoc (fun l r => (l <+> " " <+> r)) xs.toList
def hsep (xs : Array Doc) : Doc := foldDoc (fun l r => (l <> " " <> alignDoc r)) xs.toList
def vcat (xs : Array Doc) : Doc := foldDoc (fun l r => l <> Doc.hardNl <> r) xs.toList
-- let hsep  = fold_doc (fun x y -> x <+> space <+> y)
def mysep (xs : Array Doc) : Doc :=  (hsep xs) <^> (vcat xs)

-- Pretty-print an S-expression as a FormatM Doc
partial def pp : SExpr → FormatM Doc
  | atom s => return toDoc s
  | list xs => do
    let docs ← xs.mapM pp
    fmt (toDoc "(" <> alignDoc (mysep docs <> toDoc ")"))

partial def convert (v:Json) : SExpr :=
  match v with
  | .str s => atom s
  | .arr xs => list (Array.map convert xs)
  | _ => panic! "bad input"

-- Benchmark runner in FormatM style
def sexprBenchmark (doc : Doc) (state : FormatState) (c : Config):  IO Info := do
  let out := doc.print DefaultCost (cacheSize := state.nextId) (col := 0) (widthLimit := c.pageWidth) (computationWidth := c.computationWidth)

  return {
    out := out.layout,
    cost := s!"width:{out.cost.widthCost} height:{out.cost.lineCost}",
    isTainted := false
  }

-- Main entry point
def main (originalArgs : List String) : IO Unit := do
  let cfg ← parseArgs originalArgs

  let path := match (← IO.getEnv "BENCHDATA") with
  | some s => s
  | _ => "../data" -- during development
  let path := path ++ (s!"/random-tree-{cfg.size}.sexp")
  let content ← IO.FS.readFile (path)
  let (doc, state) := match Json.parse content with
  | Except.ok json =>
    simpleFormattingContext (pp (convert json)) 4
  | Except.error _ => simpleFormattingContext (return "no" <_> "json") 6

  runBenchmark "sexp-random" cfg (sexprBenchmark doc state)
