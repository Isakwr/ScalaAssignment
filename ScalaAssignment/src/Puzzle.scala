// Puzzle.scala

// The puzzle structure represents a static snapshot of the grid and clues, which do not change
// after they are initialized. Once the puzzle is set up, the solver works with the fixed clues
// and grid structure to derive a solution. Since the puzzle itself doesn't need to be modified
// during solving (only the solution changes), immutability is a natural fit. It seems more
// logical, then to use a case class here, rather than a basic class, since it provides
// immutability by default, as well as built-in functionalities that will be helpful for
// modeling  the fixed data of the puzzle.

// definition of Puzzle case class, including fields for the grid, clues, etc.

import scala.util.Random

case class Puzzle(
                 size: (Int, Int),         // (width, height) of the puzzle
                 grid: Array[Array[Char]],  // 2D array representing the grid
                 rowClues: List[Int],       // clues for each row
                 columnClues: List[Int]     // clues for each column
                 )

// companion object to provide utility methods related to Puzzle
object Puzzle {
  // solve method that that fills empty squares with random numbers
  def solve(puzzle: Puzzle): Solution = {
    val (width, height) = puzzle.size
    val random = new Random()

    // creates a new grid with the solved puzzle
    val solvedGrid = puzzle.grid.map(row =>
      row.map {
        case '_' => (random.nextInt(width) + 1).toString.charAt(0) // replace '_' with a random number
        case ch => ch                                  // leave other characters unchanged
      }
    )

    Solution(solvedGrid)
  }
}

// solution case class to represent a Solution to a Puzzle
case class Solution(grid: Array[Array[Char]]) {
  // convert the grid to a string for output
  override def toString: String =
    grid.map(_.mkString(" ")).mkString("\n")
}