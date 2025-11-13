import EC2025.ECUtils
open Std Batteries

namespace Quest06

def quest := "06"

def parse (raw : String) : List Char :=
  raw.toList

def part1 (l : List Char) : Nat :=
  l.all_pairs.countP (fun (c1, c2) => c1 == 'A' && c2 == 'a')

def part2 (l : List Char) : Nat :=
  l.all_pairs.countP (fun (c1, c2) => c1.toLower == c2 && c1.isUpper)

def part3 (l : List Char) : Nat :=
  let n := l.length
  let l := l.toArray
  let single (i : Nat) :=
    let i : Int := i
    List.range (2 * 1000 + 1)
    |>.map (fun (j : Nat) => (i, i + j - 1000))
    |>.filter (
      fun (i, j) =>
        let c1 := l[i.toNat]!
        let c2 := l[(j % n).toNat]!
        c1.toLower == c2 && c1.isUpper
    ) |>.map (
      -- intentional Nat subtraction
      fun (i, j) => 1000 - (i / n - j / n).natAbs
    ) |>.sum
  List.range n |>.map single |>.sum

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest06
