import EC2025.ECUtils
import Std
import Batteries

def quest := "01"

namespace Quest01

def parse_step (op : String) :=
  (if String.Pos.Raw.get! op 0 == 'R' then 1 else -1) * (op.drop 1).toInt!

def parse (raw : String) : List String × List Int :=
  let lines := raw.splitOn "\n"
  (lines[0]!.splitOn ",", (lines[2]!.splitOn ",").map parse_step)

def part1' (names : List String) (ops : List Int) : String :=
  let step : Int → Int → Int :=
    fun i di => min (max (i + di) 0) (names.length - 1)
  let i := ops.foldl step 0
  names[i.toNat]!

def part1 (input : String) := part1'.uncurry (parse input)

def part2' (names : List String) (ops : List Int) : String :=
  let step : Int → Int → Int :=
    fun i di => (i + di) % names.length
  let i := ops.foldl step 0
  names[i.toNat]!

def part2 (input : String) := part2'.uncurry (parse input)

def part3' (names : List String) (ops : List Int) : String :=
  let step : List String → Int → List String :=
    fun names i =>
      let i := (i % names.length).toNat
      names.set 0 names[i]! |>.set i names[0]!
  let names := ops.foldl step names
  names[0]!

def part3 (input : String) := part3'.uncurry (parse input)

def solution := ECSolution.mk part1 part2 part3

def main := solution.run

end Quest01
