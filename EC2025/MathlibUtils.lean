/-! These are utilities that are useful for computation, but are only defined in Mathlib.
If Mathlib is imported, this file should not be imported. -/

def List.prod {α} [One α] [Mul α] (l : List α) : α :=
  l.foldl (· * ·) 1
