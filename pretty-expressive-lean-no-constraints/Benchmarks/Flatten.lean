import BenchTool
import BaseFormatter

open PrettyFormat

def quadratic : Nat → FormatM Doc
| 0 => return toDoc "line"
| n + 1 => do
  fmt ((← quadratic n).group <> Doc.nl <> "line")

def flattenBenchmark (doc : Doc) (state : FormatState) (c : Config):  IO Info := do
  let out := doc.print DefaultCost (cacheSize := state.nextId) (col := 0) (widthLimit := c.pageWidth) (computationWidth := c.computationWidth)

  return {
    out := out.layout,
    cost := s!"width:{out.cost.widthCost} height:{out.cost.lineCost}",
    isTainted := false
  }

-- Main entry point
def main (originalArgs : List String): IO Unit := do
  let cfg ← parseArgs originalArgs
  let (doc, state) := simpleFormattingContext (quadratic cfg.size) 4
  runBenchmark "flatten" cfg (flattenBenchmark doc state)
