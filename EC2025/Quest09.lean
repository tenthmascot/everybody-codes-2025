import EC2025.ECUtils
open Std Batteries

namespace Quest09

def quest := "09"

structure Duck where
  id : Nat
  dna : List Char

namespace Duck
def sim (d1 d2 : Duck) : Nat :=
  d1.dna.zip d2.dna |>.countP (fun (x, y) => x == y)

def isChild (c p1 p2 : Duck) : Bool :=
  c.dna.zip (p1.dna.zip p2.dna) |>.all (fun (x, (y, z)) => x == y || x == z)

def simScore (c p1 p2 : Duck) : Nat :=
  if c.isChild p1 p2 then c.sim p1 * c.sim p2 else 0

end Duck

def parse_single (line : String) : Duck :=
  let words := line.splitOn ":"
  ⟨words[0]!.toNat!, words[1]!.toList⟩

def parse (raw : String) : List Duck :=
  raw.splitOn "\n" |>.map parse_single

def part12 (l : List Duck) : Nat :=
  l.product l.all_pairs
  |>.filter (fun (c, p1, p2) => c.id != p1.id && c.id != p2.id)
  |>.map (fun (c, p1, p2) => c.simScore p1 p2) |>.sum

def part3 (_ : List Duck) := "not implemented"

def solution := ECSolution.mkOfParse parse part12 part12 part3

def main := solution.run

end Quest09
