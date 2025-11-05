import EC2025.ECUtils
open Std Batteries

namespace Quest01

def quest := "01"

def parse_step (op : String) :=
  (if String.Pos.Raw.get! op 0 == 'R' then 1 else -1) * (op.drop 1).toInt!

structure Data where
  names : List String
  ops : List Int

def parse (raw : String) : Data :=
  let lines := raw.splitOn "\n"
  ⟨lines[0]!.splitOn ",", (lines[2]!.splitOn ",").map parse_step⟩

def part1 (data : Data) : String :=
  let ⟨names, ops⟩ := data
  let step : Int → Int → Int :=
    fun i di => min (max (i + di) 0) (names.length - 1)
  let i := ops.foldl step 0
  names[i.toNat]!

def part2 (data : Data) : String :=
  let ⟨names, ops⟩ := data
  let step : Int → Int → Int :=
    fun i di => (i + di) % names.length
  let i := ops.foldl step 0
  names[i.toNat]!

def part3 (data : Data) : String :=
  let ⟨names, ops⟩ := data
  let step : List String → Int → List String :=
    fun names i =>
      let i := (i % names.length).toNat
      names.set 0 names[i]! |>.set i names[0]!
  let names := ops.foldl step names
  names[0]!

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest01
