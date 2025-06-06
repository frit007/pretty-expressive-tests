import BenchTool
import BaseFormatter

open PrettyFormat

def fill_sep (xs : List String): FormatM Doc :=
  match xs with
  | [] => return toDoc ""
  | x :: xs =>
    loop xs (toDoc x)
where
  loop (xs : List String) (acc: Doc) : FormatM Doc := do
    match xs with
    | [] => return acc
    | x :: xs =>
      -- note: we must explicitly call fmt, because otherwise we do not cache
      loop xs (← fmt ((acc <+> (" " <+> x)) <^> (acc <$$> x)))

def fillBenchmark (c : Config):  IO Info := do
  let path := match (← IO.getEnv "BENCHDATA") with
  | some s => s
  | _ => "../data"
  let path := path ++ "/words"
  let content ← IO.FS.readFile (path)
  let allLines := content.splitOn "\n"
  let lines := allLines.take c.size -- unfortunate that we pay for reading the entire file
  let doc := fill_sep lines
  let (doc, state) := simpleFormattingContext (doc)

  let a ← doc.prettyPrint DefaultCost (cacheSize := state.nextId) (col := 0) (widthLimit := c.pageWidth) (computationWidth := c.computationWidth)
  return {
    out := a,
    cost := "0",
    isTainted := false
  }

-- Main entry point
def main (originalArgs : List String): IO Unit := do
  let cfg ← parseArgs originalArgs
  runBenchmark "fill-sep" cfg fillBenchmark
