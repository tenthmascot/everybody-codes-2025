import EC2025.ECUtils
open Std Batteries

namespace Quest10

def quest := "10"

structure Data where
  grid : Std.HashMap (Int × Int) Char
  dragon : Int × Int
  sheeps : HashSet (Int × Int)
  hideouts : HashSet (Int × Int)

def dragonMoves := [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]

def parse (raw : String) : Data :=
  let grid := raw.splitOn "\n"
    |>.zipIdx.flatMap (
      fun (line, r) => line.toList.zipIdx.map (fun (ch, c) => ((r, c), ch))
    ) |> Std.HashMap.ofList
  {
    grid := grid
    dragon := grid.filter (fun _ => (· == 'D')) |>.keys.head!,
    sheeps := ⟨grid.filter (fun _ => (· == 'S')) |>.map (fun _ _ => ())⟩,
    hideouts := ⟨grid.filter (fun _ => (· == '#')) |>.map (fun _ _ => ())⟩
  }

def dragonExpand (dragons : HashSet (Int × Int)) : HashSet (Int × Int) :=
  dragons.toList.product dragonMoves
    |>.map (fun ((vr, vc), (dr, dc)) => (vr + dr, vc + dc))
    |> HashSet.ofList

def dragonLocations (dragon : Int × Int) (n : Nat) : HashSet (Int × Int) := match n with
| 0 => {dragon}
| n+1 => dragonExpand (dragonLocations dragon n)

def dragonLocationsUpTo (dragon : Int × Int) (n : Nat) : HashSet (Int × Int) := match n with
| 0 => {dragon}
| n+1 => dragonExpand (dragonLocationsUpTo dragon n) |>.insert dragon

def part1_moves := 4

def part1 (data : Data) : Nat :=
  dragonLocationsUpTo data.dragon part1_moves
    |>.toList.countP (· ∈ data.sheeps)

structure Part2Info where
  grid : Std.HashMap (Int × Int) Char
  dragons : HashSet (Int × Int)
  sheeps : HashSet (Int × Int)
  hideouts : HashSet (Int × Int)
  sheepsEaten : Nat

def part2_cycle (info : Part2Info) : Part2Info :=
  let dragons := info.dragons
  let sheeps := info.sheeps
  let sheepsEaten := info.sheepsEaten

  let dragons := dragonExpand dragons |>.filter (· ∈ info.grid)
  let sheepsGone := sheeps.filter
    (fun sheep => sheep ∈ dragons && sheep ∉ info.hideouts)
  let sheepsEaten := sheepsEaten + sheepsGone.size
  let sheeps := sheepsGone.fold (·.erase ·) sheeps

  let sheeps := sheeps.toList.map (fun (r, c) => (r + 1, c))
    |>.filter (· ∈ info.grid) |> HashSet.ofList
  let sheepsGone := sheeps.filter
    (fun sheep => sheep ∈ dragons && sheep ∉ info.hideouts)
  let sheepsEaten := sheepsEaten + sheepsGone.size
  let sheeps := sheepsGone.fold (·.erase ·) sheeps

  {
    grid := info.grid,
    dragons := dragons,
    sheeps := sheeps,
    hideouts := info.hideouts
    sheepsEaten := sheepsEaten
  }

def part2_moves := 20

def part2 (data : Data) : Nat :=
  part2_moves.repeat part2_cycle {
    grid := data.grid,
    dragons := {data.dragon},
    sheeps := data.sheeps,
    hideouts := data.hideouts,
    sheepsEaten := 0
  } |>.sheepsEaten

inductive Part3Turn where
| dragon : Part3Turn
| sheep : Part3Turn
deriving DecidableEq, Hashable

structure Part3Data where
  dragon : Int × Int
  sheeps : List (Int × Int)
  turn : Part3Turn
deriving DecidableEq, Hashable

def part3_aux
  (grid : Std.HashMap (Int × Int) Char)
  (hideouts : HashSet (Int × Int))
  (fuel : Nat)
  (data : Part3Data × Std.HashMap Part3Data Nat) :
    Nat × Std.HashMap Part3Data Nat :=
  match fuel with
  | 0 => panic! s!"quest {quest} part 3 ran out of fuel"
  | fuel+1 =>
  let (data, store) := data
  if H : data ∈ store then (store[data], store) else
  let dragon := data.dragon
  let sheeps := data.sheeps
  let turn := data.turn

  let (ans, store) := if sheeps.isEmpty then (1, store) else match turn with
    | .dragon => Id.run do
      let dragons := dragonExpand {dragon} |>.filter (· ∈ grid)
      let mut ans := 0
      let mut store := store
      for dragon in dragons do
        let newSheeps := if dragon ∈ hideouts then sheeps else sheeps.erase dragon
        let out := part3_aux grid hideouts fuel (
          {dragon := dragon, sheeps := newSheeps, turn := .sheep},
          store
        )
        (ans, store) := (ans + out.1, out.2)
      (ans, store)
    | .sheep => Id.run do
      let mut canMove := false
      let mut ans := 0
      let mut store := store
      for sheep in sheeps do
        let newSheep := (sheep.1 + 1, sheep.2)
        if newSheep ∉ grid || (newSheep ∈ hideouts || newSheep != dragon) then
          canMove := true
        if newSheep ∈ grid && (newSheep ∈ hideouts || newSheep != dragon) then
          let newSheeps := sheeps.replace sheep newSheep |>.mergeSort (Prod.lexLt · ·)
          let out := part3_aux grid hideouts fuel (
            {dragon := dragon, sheeps := newSheeps, turn := .dragon},
            store
          )
          (ans, store) := (ans + out.1, out.2)
      if !canMove then
        (ans, store) := part3_aux grid hideouts fuel (
          {dragon := dragon, sheeps := sheeps, turn := .dragon},
          store
        )
      (ans, store)
  (ans, store.insert data ans)

def part3 (data : Data) : Nat :=
  part3_aux data.grid data.hideouts (fuel := 10 * data.grid.size) ({
      dragon := data.dragon,
      sheeps := data.sheeps.toList.mergeSort (Prod.lexLt · ·),
      turn := .sheep
  }, ∅) |>.1

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest10
