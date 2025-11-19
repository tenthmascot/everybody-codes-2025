import EC2025.ECUtils
open Std Batteries

namespace Quest12

def quest := "12"

def parse (raw : String) : Std.HashMap (Int × Int) Int :=
  raw.splitOn "\n"
    |>.zipIdx.flatMap (
      fun (line, r) =>
        line.toList.zipIdx.map (fun (ch, c) => ((r, c), ch.toString.toInt!))
    ) |> Std.HashMap.ofList

def part123_aux'
    (grid : Std.HashMap (Int × Int) Int)
    (todo : List (Int × Int))
    (seen : HashSet (Int × Int))
    (fuel : Nat) : HashSet (Int × Int) := match fuel with
  | 0 => panic! s!"quest {quest} aux ran out of fuel"
  | fuel+1 => match todo with
    | [] => seen
    | p :: todo =>
      if p ∈ seen then part123_aux' grid todo seen fuel else
      let seen := seen.insert p
      let (pr, pc) := p
      let nps := List.padj4.map (
        fun (dr, dc) => (pr + dr, pc + dc)
      ) |>.filter (
        fun np => np ∈ grid && np ∉ seen && grid[np]! ≤ grid[p]!
      )
      part123_aux' grid (nps ++ todo) seen fuel

def part12_aux
    (grid : Std.HashMap (Int × Int) Int)
    (todo : List (Int × Int)) : Nat :=
  part123_aux' grid todo ∅ (fuel := 100 * grid.size) |>.size

def part3_cycle (grid : Std.HashMap (Int × Int) Int) : Std.HashMap (Int × Int) Int :=
  let data := grid.keys.map (fun p => part123_aux' grid [p] ∅ (fuel := 100 * grid.size))
  let best := data.foldl (
    fun s1 s2 => if s1.size < s2.size then s2 else s1
    ) ∅
  grid.filter (fun p _ => p ∉ best)

def part1 (grid : Std.HashMap (Int × Int) Int) : Nat :=
  part12_aux grid [(0, 0)]

def part2 (grid : Std.HashMap (Int × Int) Int) : Nat :=
  let top := grid.keys.foldl
    (fun p q => if Prod.lexLt p q then q else p)
    (0, 0)
  part12_aux grid [(0, 0), top]

def part3_num_cycles := 3

def part3 (grid : Std.HashMap (Int × Int) Int) : Nat :=
  let safe := part3_num_cycles.repeat part3_cycle grid
  -- okay Nat subtraction: safe ⊆ grid
  grid.size - safe.size

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest12
