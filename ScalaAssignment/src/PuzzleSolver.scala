// PuzzleSolver.scala

object PuzzleSolver {
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Usage: PuzzleSolver <input file> <output file>")
      System.exit(1)
    }

    val inputFile = args(0)
    val outputFile = args(1)
    
    // read all puzzles from the input file
    val puzzles = PuzzleReaderWriter.readPuzzles(inputFile)

    // solve each puzzle and gather the solutions
    val solutions = puzzles.map { puzzle =>
      for (rowIndex <- puzzle.grid.indices) {
        if (PuzzleChecker.isFullRow(puzzle, rowIndex)) {
          println(s"Row $rowIndex is complete!")
        }
      }

      for (colIndex <- puzzle.grid.head.indices) {
        if (PuzzleChecker.isFullColumn(puzzle, colIndex)) {
          println(s"Column $colIndex is complete!")
        }
      }

      Puzzle.solve(puzzle)
    }

    // write all the solutions to the output file at once
    PuzzleReaderWriter.writeSolution(outputFile, solutions)
  }
}
