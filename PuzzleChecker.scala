// PuzzleChecker.scala

object PuzzleChecker {

  // method to check if any row is fully filled according to the row clues
  def isFullRow(puzzle: Puzzle, rowIndex: Int): Boolean = {
    val clue = puzzle.rowClues(rowIndex)
    val width = puzzle.size._1
    width == clue // check if the width matches the clue
  }

  // method to check if any column is fully filled according to the column clues
  def isFullColumn(puzzle: Puzzle, colIndex: Int): Boolean = {
    val clue = puzzle.columnClues(colIndex)
    val height = puzzle.size._2
    height == clue // check if the number of filled blocks matches the clue
  }
}
