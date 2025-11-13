import Batteries
import EC2025.MathlibUtils -- if Mathlib is imported, this should not be imported
import EC2025.Utils.Counter
open Std Batteries

/-- Returns the list `l` without duplicates, in some order.
Uses a HashSet for faster performance over the `List.dedup` function in Mathlib. -/
def List.fast_dedup {α} [BEq α] [Hashable α] (l : List α) : List α :=
  HashSet.ofList l |>.toList

/--
Returns successive adjacent pairs from a list.

Examples:
* `[].adj_pairs = []`
* `[1].adj_pairs = []`
* `[1, 2, 3, 4].adj_pairs = [(1, 2), (2, 3), (3, 4)]`
-/
def List.adj_pairs {α} (l : List α) : List (α × α) := match l with
| [] => []
| [_] => []
| a :: b :: as => (a, b) :: (b :: as).adj_pairs

/--
Returns all sublists of length 2 from a list as pairs.
Equivalent to Mathlib's `List.sublistsLen 2`, but with a list of pairs.

Examples:
* `[].all_pairs = []`
* `[1, 2].all_pairs = [(1, 2)]`
* `[1, 2, 3, 4].all_pairs = [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]`
-/
def List.all_pairs {α} (l : List α) : List (α × α) := match l with
| [] => []
| a :: as => as.map (Prod.mk a ·) ++ as.all_pairs
