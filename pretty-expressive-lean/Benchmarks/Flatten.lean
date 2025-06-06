import BenchTool
import BaseFormatter

open PrettyFormat

def quadratic : Nat → FormatM Doc
| 0 => return toDoc "line"
| n + 1 => do
  fmt ((← quadratic n).group <> Doc.nl <> "line")

def flattenBenchmark (c : Config):  IO Info := do
  let (doc, state) := simpleFormattingContext (quadratic c.size)

  let a ← doc.prettyPrint DefaultCost (cacheSize := state.nextId) (col := 0) (widthLimit := c.pageWidth) (computationWidth := c.computationWidth)

  return {
    out := a,
    cost := "0",
    isTainted := false
  }

-- Main entry point
def main (originalArgs : List String): IO Unit := do
  let cfg ← parseArgs originalArgs
  runBenchmark "flatten" cfg flattenBenchmark
