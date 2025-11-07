# EC2025

These are my solutions for [Everybody Codes 2025](https://everybody.codes/event/2025/quests) in Lean 4.

## Usage

Place your input notes (puzzle data) in [`input`](input). See [that folder's README](input/README.md) for more details.

Run `lake exe ec2025` to output answers to all quests. To only output answers for a single quest, use the quest number as the sole argument. (For example, to only output answers for quest 1, run `lake exe ec2025 1`.)

This project does not have Mathlib as a dependency. If you want to include Mathlib, then you must uncomment `import EC2025.MathlibUtils` from [`Utils.lean`](EC2025/Utils.lean), as [`MathlibUtils.lean`](EC2025/MathlibUtils.lean) duplicates functions from Mathlib.
