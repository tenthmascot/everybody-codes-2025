import EC2025.ECUtils
open Std Batteries

namespace Quest13

def quest := "13"

def parse_single (line : String) : Int × Int :=
  match line.splitOn "-" |>.map String.toInt! with
  | [] => panic! s!"invalid data in quest {quest}"
  | [a] => (a, a)
  | a :: b :: _ => (a, b)

def parse (raw : String) : Array (Int × Int) :=
  (raw.splitOn "\n" |>.toArray.map parse_single)

def prepare (raw : String) : List ((Int × Int) × Int) :=
  let ranges := parse raw
  let left := [0:ranges.size:2].toList.map
    (fun i => ranges[i]?)
    |>.reduceOption.map ((·, 1))
  let right := [1:ranges.size:2].toList.map
    (fun i => ranges[i]?.map Prod.swap)
    |>.reduceOption.map ((·, -1))
  [((1, 1), 1)] ++ left ++ right.reverse

def part123_aux' (l : List ((Int × Int) × Int)) (i : Nat) : Int :=
  match l with
  | [] => panic! s!"quest {quest} part123_aux failed"
  | ((a, b), d) :: l =>
    let size := (a - b).natAbs + 1
    if size <= i
      -- intentional Nat subtraction
      then part123_aux' l (i - size)
    else
      a + i * d

def part123_aux (l : List ((Int × Int) × Int)) (i : Nat) : Int :=
  let total := l.map (fun ((a, b), _) => (a - b).natAbs + 1) |>.sum
  part123_aux' l (i % total)

def part1 (l : List ((Int × Int) × Int)) : Int :=
  part123_aux l 2025

def part2 (l : List ((Int × Int) × Int)) : Int :=
  part123_aux l 20252025

def part3 (l : List ((Int × Int) × Int)) : Int :=
  part123_aux l 202520252025

def solution := ECSolution.mkOfParse prepare part1 part2 part3

def main := solution.run

end Quest13
