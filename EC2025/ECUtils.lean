import EC2025.SolutionUtils

/-- An Everybody Codes solution, consisting of three `SolutionPart`s. -/
structure ECSolution {α : Type u} {β : Type v} {γ : Type w} where
  part1 : @SolutionPart α
  part2 : @SolutionPart β
  part3 : @SolutionPart γ

/-- An Everybody Codes input, consisting of three `System.FilePath`s. -/
structure ECInput where
  file1 : System.FilePath
  file2 : System.FilePath
  file3 : System.FilePath

/-- The folder containing the input notes. -/
def inputFolder : System.FilePath := "input"

/-- The filename corresponding to a particular quest and part. -/
def fileOfQuestPart (quest part : String) : System.FilePath :=
  inputFolder / s!"{quest}.{part}"

/-- The `ECInput` corresponding to a particular quest. -/
def ECInputOfQuest (quest : String) : ECInput where
  file1 := fileOfQuestPart quest "1"
  file2 := fileOfQuestPart quest "2"
  file3 := fileOfQuestPart quest "3"

/-- Runs a solution, printing all three answers. -/
def ECSolution.run {α β γ} (solution : @ECSolution α β γ) [ToString α] [ToString β] [ToString γ]
    (input : ECInput := by exact ECInputOfQuest quest) := do
  solution.part1.run "Part 1" input.file1
  solution.part2.run "Part 2" input.file2
  solution.part3.run "Part 3" input.file3
