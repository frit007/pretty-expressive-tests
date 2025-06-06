import BenchTool
import BaseFormatter

open PrettyFormat

def pp : Nat → FormatM Doc
| 0 => return toDoc ""
| n + 1 =>
  return "line" <> (← pp n)

def concatBenchmark (c : Config):  IO Info := do
  let (doc, state) := simpleFormattingContext (pp c.size)

  let a ← doc.prettyPrint DefaultCost (cacheSize := state.nextId) (col := 0) (widthLimit := c.pageWidth) (computationWidth := c.computationWidth)

  return {
    out := a,
    cost := "0",
    isTainted := false
  }

-- Main entry point
def main (originalArgs : List String): IO Unit := do
  let cfg ← parseArgs originalArgs
  runBenchmark "concat" cfg concatBenchmark
