import EC2025.ECUtils
open Std Batteries

namespace Quest02

def quest := "02"

def parse (raw : String) : Int × Int :=
  let ints := raw.drop 3 |>.dropRight 1
    |>.splitOn "," |>.map String.toInt!
  (ints[0]!, ints[1]!)

def step (A : Int × Int) (div : Int) (val : Int × Int) : Int × Int :=
  (val.1 ^ 2 - val.2 ^ 2, 2 * val.1 * val.2)
  |>.map (·.tdiv div + A.1) (·.tdiv div + A.2)

def part1 (A : Int × Int) : String :=
  let out := (3).repeat (step A 10) (0, 0)
  s!"[{out.1},{out.2}]"

def part23_safeAux (A val : Int × Int) (n : Nat) : Bool := match n with
  | 0 => true
  | n+1 =>
    let val' := step A 100000 val
    if max val'.1.natAbs val'.2.natAbs > 1000000
      then false
      else part23_safeAux A val' n

def part23_safe (A : Int × Int) : Bool :=
  part23_safeAux A (0, 0) 100

def part2 (A : Int × Int) : Nat :=
  let as := (List.range 101).map (A.1 + 10 * · : Nat → Int)
  let bs := (List.range 101).map (A.2 + 10 * · : Nat → Int)
  as.product bs |>.countP part23_safe

def part3 (A : Int × Int) : Nat :=
  let as := (List.range 1001).map (A.1 + · : Nat → Int)
  let bs := (List.range 1001).map (A.2 + · : Nat → Int)
  as.product bs |>.countP part23_safe

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest02
