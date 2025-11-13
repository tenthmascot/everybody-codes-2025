import EC2025.ECUtils
open Std Batteries

namespace Quest08

def quest := "08"

def parse (raw : String) : List (Nat × Nat) :=
  raw.splitOn "," |>.map String.toNat! |>.adj_pairs

def isCrossing (a b : Nat × Nat) : Bool :=
  let a := (min a.1 a.2, max a.1 a.2)
  let b := (min b.1 b.2, max b.1 b.2)
  (a.1 < b.1 && b.1 < a.2 && a.2 < b.2) ||
  (a.1 == b.1 && a.2 == b.2) ||
  (b.1 < a.1 && a.1 < b.2 && b.2 < a.2)

def part1_n := 32

def part1 (l : List (Nat × Nat)) : Nat :=
  l.countP (fun (i, j) => 2 * (i - j : Int).natAbs == part1_n)

def part23_n := 256

def part2 (l : List (Nat × Nat)) : Nat :=
  l.all_pairs.countP (fun (p, q) => isCrossing p q)

def part3 (l : List (Nat × Nat)) : Nat :=
  (List.range' 1 part23_n).all_pairs.map
    (fun p => l.countP (fun q => isCrossing p q))
  |>.max?.get!

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest08
