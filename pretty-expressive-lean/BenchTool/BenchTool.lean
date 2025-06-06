import Lean
import Std
open IO Lean
open System

structure Config where
  size : Nat := 0
  pageWidth : Nat := 80
  computationWidth : Nat := 100
  program : String := ""
  out : Option String := none
  viewCost : Bool := false
deriving Repr


def parseArgs (args : List String) : IO Config := do
  let mut cfg := { :Config }
  let mut i := 0
  while h : i < args.length do
    let arg := args.get ⟨i, h⟩
    match arg with
    | "--size" =>
      i := i + 1
      cfg := { cfg with size := (args.get? i).getD "0" |>.toNat! }
    | "--page-width" =>
      i := i + 1
      cfg := { cfg with pageWidth := (args.get? i).getD "80" |>.toNat! }
    | "--computation-width" =>
      i := i + 1
      cfg := { cfg with computationWidth := (args.get? i).getD "100" |>.toNat! }
    | "--program" =>
      i := i + 1
      cfg := { cfg with program := (args.get? i).getD "" }
    | "--out" =>
      i := i + 1
      cfg := { cfg with out := args.get? i }
    | "--view-cost" =>
      cfg := { cfg with viewCost := true }
    | _ => pure ()
    i := i + 1
  return cfg

structure Info where
  out : String
  cost : String
  isTainted : Bool

def md5Hash (input : String) : IO String := do
  let fileName := "tmpFile.txt"

  IO.FS.writeFile fileName input

  let output ← IO.Process.output {
    cmd := "md5sum",
    args := #[ fileName ]
  }

  IO.FS.removeFile fileName

  return output.stdout.splitOn " " |>.head!.trim

def runBenchmark (program:String) (cfg : Config) (benchFn : Config → IO Info) : IO Unit := do
  let start ← IO.monoNanosNow
  let result ← benchFn cfg
  let stop ← IO.monoNanosNow
  let duration := (Float.ofNat (stop - start)) / 1_000_000_000.0

  -- Handle output
  match cfg.out with
  | some "-" => IO.println result.out
  | some path => do
      FS.writeFile path result.out
  | none => pure ()

  -- View cost if requested
  if cfg.viewCost then
    IO.println s!"(cost {result.cost})"

  let lineCount := result.out.foldl (fun acc c =>
    if c == '\n' then acc + 1 else acc) 1

  let md5 ← md5Hash result.out

  let formattedOut :=
    s!"((target pretty-expressive-lean)" ++
    s!" (program {program})" ++
    s!" (duration {duration})" ++
    s!" (lines {lineCount})" ++
    s!" (size {cfg.size})" ++
    s!" (md5 {md5})" ++
    s!" (page-width {cfg.pageWidth})" ++
    s!" (computation-width {cfg.computationWidth})" ++
    s!" (cost {result.cost})" ++
    s!" (tainted? {if result.isTainted then "true" else "false"}))"

  IO.println formattedOut
