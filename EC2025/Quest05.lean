import EC2025.ECUtils
open Std Batteries

namespace Quest05

def quest := "05"

structure FishboneLevel where
  left : Option Int
  center : Int
  right : Option Int

structure Fishbone where
  id : Int
  levels : List FishboneLevel
deriving Inhabited

def Fishbone.inner.add (l : List FishboneLevel) (val : Int) := Id.run do
  for h : i in [0:l.length] do
    if val < l[i].center ∧ l[i].left.isNone then
      return l.set i ⟨val, l[i].center, l[i].right⟩
    else if val > l[i].center ∧ l[i].right.isNone then
      return l.set i ⟨l[i].left, l[i].center, val⟩
  return l ++ [⟨none, val, none⟩]

def Fishbone.make (l : List Int) : List FishboneLevel :=
  l.foldl Fishbone.inner.add []

def Fishbone.quality (f : Fishbone) : Int :=
  let s := f.levels.map (ToString.toString ∘ FishboneLevel.center)
  String.join s |>.toInt!

def FishboneLevel.value (f : FishboneLevel) : Int :=
  let left := f.left.map ToString.toString |>.getD ""
  let right := f.right.map ToString.toString |>.getD ""
  s!"{left}{f.center}{right}".toInt!

def Fishbone.levelValues (f : Fishbone) : List Int :=
  f.levels.map (·.value)

def Fishbone.value (f : Fishbone) : Int × List Int × Int :=
  ⟨f.quality, f.levelValues, f.id⟩

instance : LT Fishbone :=
  ⟨(Prod.Lex (· < ·) (Prod.Lex (· < ·) (· < ·)) ·.value ·.value)⟩

instance : DecidableLT Fishbone :=
  inferInstanceAs <| DecidableRel (fun _ _ => Prod.Lex _ _ _ _)


def parse_single (raw : String) : Fishbone :=
  let l := raw.replace ":" "," |>.splitOn "," |>.map String.toInt!
  ⟨l.head!, Fishbone.make l.tail!⟩

def parse (raw : String) : List Fishbone :=
  raw.splitOn "\n" |>.map parse_single

def part1 (l : List Fishbone) : Int :=
  l.head!.quality

def part2 (l : List Fishbone) : Int :=
  let qs := l.map Fishbone.quality
  qs.max?.merge (· - ·) qs.min? |>.get!

def part3 (l : List Fishbone) : Int :=
  l.mergeSort (· > ·) |>.zipIdx 1
    |>.map (fun (f, i) => f.id * i) |>.sum

def solution := ECSolution.mkOfParse parse part1 part2 part3

def main := solution.run

end Quest05
