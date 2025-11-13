import EC2025.ECUtils
open Std Batteries

namespace Quest07

def quest := "07"

structure Data where
  names : List String
  rules : Std.HashMap Char (HashSet Char)

def parse_rule_single (raw : String) : Std.HashMap Char (HashSet Char) :=
  let fst := String.Pos.Raw.get raw 0
  let snds := raw.splitOn " "
    |>.getLast!.splitOn ","
    |>.map (String.Pos.Raw.get · 0)
  {(fst, HashSet.ofList snds)}

def parse (raw : String) : Data :=
  let lines := raw.splitOn "\n"
  let names := lines[0]!.splitOn ","
  let lines := lines.drop 2
  let rules := lines.map parse_rule_single
    |>.foldl HashMap.union ∅
  ⟨names, rules⟩

def name_valid (data : Data) (name : String) : Bool :=
  name.toList.adj_pairs.all (
    fun (c1, c2) => if H : data.rules.contains c1
      then data.rules[c1].contains c2
      else false
  )

def part1 (data : Data) : String :=
  data.names.filter (name_valid data) |>.head!

def part2 (data : Data) : Nat :=
  data.names.zipIdx 1 |>.filter (name_valid data ·.1)
  |>.map (·.2) |>.sum

def part3_min_length := 7
def part3_max_length := 11

def part3_dp (data : Data) : Array (Std.HashMap Char Nat) :=
  let letters := data.rules.keys
    ++ (data.rules.values.map HashSet.toList |>.flatten)
    |>.fast_dedup
  (part3_max_length - 1).repeat (
    fun dpArray =>
      let dp := dpArray.back!
      let counts := letters.map
        (fun ch =>
          (data.rules.getD ch ∅).toList
          |>.map (fun ch2 => dp.getD ch2 0)
          |>.sum
        )
      let dp2 := letters.zip counts |> Std.HashMap.ofList
      dpArray.push dp2
  ) #[∅, letters.map (fun ch => (ch, 1)) |> Std.HashMap.ofList]

def part3_potential_prefixes (data : Data) : List String :=
  let prefixes := data.names
  let ignores := prefixes.product prefixes
    |>.filter (fun (n1, n2) => n1.startsWith n2 && n1 != n2)
    |>.map (·.1)
    |> HashSet.ofList
  ignores.fold HashSet.erase (HashSet.ofList prefixes)
    |>.filter (fun pfx => name_valid data pfx)
    |>.filter (fun pfx => pfx.length ≤ part3_max_length)
    |>.toList

def part3 (data : Data) : Nat :=
  let prefixes := part3_potential_prefixes data
  let dp := part3_dp data
  prefixes.map (
    fun pfx => (
      [part3_min_length + 1 - pfx.length:part3_max_length + 1 - pfx.length + 1]
      |>.toList.map (dp.getD · ∅ |>.getD pfx.back 0)
      |>.sum
    )
  ) |>.sum

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest07
