@main def PuzzleSolver(): Unit = {

  val inFile: String = "C:/Users/isakw/ikt212/ScalaAssignment/isakwr/ScalaAssignment/input.txt"
  val outFile: String = "C:/Users/isakw/ikt212/ScalaAssignment/isakwr/ScalaAssignment/output.txt"
  
    // read all puzzles from the input file
    val puzzles = PuzzleReaderWriter.readPuzzles(inFile)

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

    PuzzleReaderWriter.writeSolution(outFile, solutions)
}

