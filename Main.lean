import EC2025

/-- A list of the main definitions of all quests. -/
def mains := [
  Quest01.main,
  Quest02.main,
]

def main : IO Unit := do
  for main in mains.intersperse (IO.println "") do
    main
