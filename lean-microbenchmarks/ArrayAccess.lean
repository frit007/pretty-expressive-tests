
import Std

open Std

abbrev Bridge := Nat


def generateSortedPairs (n : Nat) (start : Bridge := 0) : IO (Array Bridge) := do
  let mut a : Array Bridge := #[]
  for i in [0:n] do
    a := a.push (start +  (2 * i) - 1)
  return a


structure wrapper where
  val : Bridge
  v : String -- force pointer
deriving Inhabited

def generateSortedPairs' (n : Nat) (start : Bridge := 0) : IO (Array wrapper) := do
  let mut a : Array (wrapper) := #[]
  for i in [0:n] do
    a := a.push ({val := start +  (2 * i) - 1, v:=s!"{n}"})
  return a

def measureTime (f : Unit → IO α) : IO (α × Nat):= do
  let before ← IO.monoNanosNow
  let res ← f ()
  let after ← IO.monoNanosNow
  return (res, after - before)

def sumSimple (acc a:Bridge) : Bridge:= acc + a
def sumWrapper (acc:Bridge)( a:wrapper) : Bridge:= acc + a.val

partial def sumSimpleFull (arr: Array Bridge) (i:Nat) (acc:Bridge) : Bridge:=
  if i >= arr.size then
    acc
  else
    sumSimpleFull arr (i + 1) (acc + arr[i]!)

partial def sumWrapperFull (arr: Array wrapper) (i:Nat) (acc:Bridge) : Bridge:=
  if i >= arr.size then
    acc
  else
    sumWrapperFull arr (i + 1) (acc + arr[i]!.val)

def shuffle [Inhabited α] (arr : Array α) : IO (Array α) := do
  let mut a := arr
  let mut m := a.size
  let mut i := m - 1

  while i > 0 do
    let j ← IO.rand 0 i
    let prev := a[j]!
    let new := a[i]!
    a := a.set! i (prev)
    a := a.set! j (new)
    i := i - 1

  return a

def main (a : List String): IO UInt32 := do
  let dataScalar ← generateSortedPairs 400000 (a.length)
  IO.println "start shuffle scalar"
  let (dataScalarRandom, shuffleScalar) ← measureTime (fun _ => shuffle dataScalar)
  IO.println s!"end shuffle scalar {shuffleScalar}"
  -- let lhs' ←

  -- let rhs ← generateSortedPairs 40000 (1 + a.length.toUInt64)
  let dataTupleSorted ← generateSortedPairs' 400000 (a.length)
  IO.println "start shuffle"
  -- let plhs' ← fisherYatesShuffle plhs
  let (dataTupleRandom, shufleTuple) ← measureTime (fun _ => shuffle dataTupleSorted)
  IO.println s!"end shuffle tuple {shufleTuple}"
  -- IO.println s!"{repr dataScalar}"
  -- panic! "aaa"

  let mut scalarLinear := 0
  let mut scalarRandom := 0
  let mut tupleLinear := 0
  let mut tupleRandom := 0
  for _ in [0:1000] do
    let (r1, t1) ← measureTime (fun _ => do return sumSimpleFull dataScalar 0 0)
    let (r2, t2) ← measureTime (fun _ => do return sumSimpleFull dataScalarRandom 0 0)
    let (r3, t3) ← measureTime (fun _ => do return sumWrapperFull dataTupleSorted 0 0)
    let (r4, t4) ← measureTime (fun _ => do return sumWrapperFull dataTupleRandom 0 0)

    IO.println s!"Scalar linear:         {t1} ns {r1}"
    IO.println s!"Scalar random:         {t2} ns {r2}"
    IO.println s!"tuple indexing sorted: {t3} ns {r3}"
    IO.println s!"tuple indexing random: {t4} ns {r4}"
    IO.println ""
    scalarLinear := scalarLinear + t1
    scalarRandom := scalarRandom + t2
    tupleLinear := tupleLinear + t3
    tupleRandom := tupleRandom + t4

  IO.println s!"scalarLinear: {scalarLinear}"
  IO.println s!"scalarRandom: {scalarRandom}"
  IO.println s!"tupleLinear:  {tupleLinear}"
  IO.println s!"tupleRandom:  {tupleRandom}"

  return 0

#eval main []
