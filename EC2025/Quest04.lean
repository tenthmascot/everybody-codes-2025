import EC2025.ECUtils
open Std Batteries

namespace Quest04

def quest := "04"

def parse (raw : String) : List Int :=
  raw.replace "|" "\n" |>.splitOn "\n"
  |>.map String.toInt!

def part1 (a : List Int) : Int :=
  2025 * a.head! / a.getLast!

def part2 (a : List Int) : Int :=
  Rat.divInt (10000000000000 * a.getLast!) a.head! |>.ceil

-- TODO make this better
def part3 (a : List Int) : Int :=
  let (as, bs) := a.zipIdx.partition (·.2 % 2 == 0)
    |>.map (List.map (·.1)) (List.map (·.1))
  100 * as.prod / bs.prod

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest04
