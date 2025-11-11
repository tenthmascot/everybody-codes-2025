import EC2025.ECUtils
open Std Batteries

namespace Quest03

def quest := "03"

def parse (raw : String) : List Int :=
  raw.splitOn "," |>.map String.toInt!

def part1 (a : List Int) : Int :=
  a.fast_dedup.sum

def part2 (a : List Int) : Int :=
  a.fast_dedup.mergeSort.take 20 |>.sum

def part3 (a : List Int) : Nat :=
  (Counter.ofList a).values.max?.getD 0

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest03
