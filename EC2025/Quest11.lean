import EC2025.ECUtils
open Std Batteries

namespace Quest11

def quest := "11"

def parse (raw : String) : Array Int :=
  raw.splitOn "\n" |>.toArray.map String.toInt!

inductive Phase where
| Phase1 : Phase
| Phase2 : Phase
deriving DecidableEq

/-- Cycle for Phase 1. -/
def cycle1 {n} (a : Vector Int n) : Vector Int n :=
  List.finRange n |>.adj_pairs.foldl
    (fun a (i, j) => if a[i] > a[j] then
      a.set i (a[i] - 1) |>.set j (a[j] + 1)
    else a) a

/-- Cycle for Phase 2. -/
def cycle2 {n} (a : Vector Int n) : Vector Int n :=
  List.finRange n |>.adj_pairs.foldl
    (fun a (i, j) => if a[i] < a[j] then
      a.set i (a[i] + 1) |>.set j (a[j] - 1)
    else a) a

def cycle {n} (data : Vector Int n × Phase) : Vector Int n × Phase :=
  let ⟨a, phase⟩ := data
  let phase := if phase == .Phase2 then phase
    else if a.adj_pairs.all (fun (x, y) => x ≤ y) then .Phase2 else phase
  let a := match phase with
  | .Phase1 => cycle1 a
  | .Phase2 => cycle2 a
  (a, phase)

def part1_cycles := 10

def part1 (a : Array Int) : Int :=
  let a := part1_cycles.repeat cycle (a.toVector, .Phase1) |>.1
  a.zipIdx 1 |>.map (fun (x, i) => x * i) |>.sum

def part2_aux {n} (data : Vector Int n × Phase) (fuel : Nat) (acc := 0) : Nat := match fuel with
  | 0 => panic! s!"quest {quest} part 2 ran out of fuel"
  | fuel+1 =>
    if data.1.adj_pairs.all (fun (x, y) => x == y) then acc
      else part2_aux (cycle data) fuel (acc + 1)

def part2 (a : Array Int) : Nat :=
  part2_aux (a.toVector, .Phase1) (fuel := 10 * (a.sum.toNat * a.size + 10))

def part3 (a : Array Int) : Nat :=
  let goal := a.sum / a.size
  a.map (· - goal |>.natAbs) |>.sum / 2

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest11
