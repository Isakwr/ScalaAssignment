// PuzzleSolver.scala

object PuzzleSolver {
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Usage: PuzzleSolver <input file> <output file>")
      System.exit(1)
    }

    val inputFile = args(0)

    // check if the output file path is absolute or relative
    val outputFile = if (args(1).startsWith("C:")) {
      args(1) // it's already an absolute path so just use it
    } else {
      s"C:/Users/evanm/ikt212g24h/assignments/solutions/ScalaAssignment/resources/${args(1)}"
    }

    // read all puzzles from the input file
    val puzzles = PuzzleReaderWriter.readPuzzles(inputFile)

    // solve each puzzle and gather the solutions
    val solutions = puzzles.map { puzzle =>
      for (colIndex <- puzzle.grid.head.indices) {
        if (PuzzleChecker.isFullColumn(puzzle, colIndex)) {
          println(s"Column $colIndex is complete!")
        }
      }

      // solve the puzzle
      Puzzle.solve(puzzle)
    }

    // write all the solutions to the output file at once
    PuzzleReaderWriter.writeSolution(outputFile, solutions)
  }
}