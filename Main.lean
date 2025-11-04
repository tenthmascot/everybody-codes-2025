import EC2025

/-- A list of the main definitions of all quests. -/
def mains := [
  Quest01.main
]

def main : IO Unit := do
  for (main, quest) in mains.zipIdx 1 do
    .println s!"Quest {quest}"
    main
    if quest < mains.length then IO.println ""
