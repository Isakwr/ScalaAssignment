// PuzzleSolver.scala

@main
def main(): Unit = {

  val inputFile = "C:/Users/naibm/ikt212/ScalaAssignment/input.txt"
  val outputFile = "C:/Users/naibm/ikt212/ScalaAssignment/output.txt"

  // read all puzzles from the input file
  val puzzles = PuzzleReaderWriter.readPuzzles(inputFile)

  // solve each puzzle and gather the solutions
  val solutions = puzzles.map { puzzle =>
    Puzzle.solve(puzzle)
  }

  // write all the solutions to the output file at once
  PuzzleReaderWriter.writeSolution(outputFile, solutions)
}
