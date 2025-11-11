import Batteries
import EC2025.MathlibUtils -- if Mathlib is imported, this should not be imported
import EC2025.Utils.Counter
open Std Batteries

/-- Returns the list `l` without duplicates, in some order.
Uses a HashSet for faster performance over the `List.dedup` function in Mathlib. -/
def List.fast_dedup {α} [BEq α] [Hashable α] (l : List α) : List α :=
  HashSet.ofList l |>.toList
