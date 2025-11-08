import EC2025

/-- A list of the main definitions of all quests. -/
def mains := [
  Quest01.main,
  Quest02.main,
  Quest03.main,
  Quest04.main,
  Quest05.main,
]

def main (args : List String) : IO Unit := do
  if args.length > 1 then
    IO.println "Too many arguments."
    IO.println "Run with no arguments to run all quests,"
    IO.println s!"or provide one argument (1~{mains.length}) to run that quest."
  else if h : args.length = 1 then
    let quest := args[0].toNat?
    if h : quest.isSome = true then
      let quest := quest.get h
      if h : 1 ≤ quest ∧ quest ≤ mains.length then
        let main := mains[quest - 1]
        main
      else
        IO.println "Quest number is out of bounds."
    else
      IO.println "Quest number is not a natural number."
  else
    for main in mains.intersperse (IO.println "") do
      main
