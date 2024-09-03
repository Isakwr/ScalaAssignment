// PuzzleReaderWriter.scala

import io.Source
import java.io.PrintWriter

object PuzzleReaderWriter {

  def readPuzzles(filename: String): List[Puzzle] = {
    // read from the file and translate to the Puzzle structure
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    var puzzles = List.empty[Puzzle]
    var i = 0

    // first line indicates the number of puzzles
    val numPuzzles = lines(i).split(" ")(1).toInt
    println(s"Number of puzzles: $numPuzzles") // prints the number of puzzles in the file
    i += 1

    // parse each puzzle
    while (i < lines.length) {
      // read the size of the puzzle (e.g., "size 4x4")
      val sizeLine = lines(i).split(" ")(1).split("x")
      val (width, height) = (sizeLine(0).toInt, sizeLine(1).toInt)
      println(s"Puzzle size: ${width}x${height}") // prints the size of each puzzle
      i += 1

      // read column clues
      val columnClues = lines(i).trim.split(" ").map(_.toInt).toList
      i += 1

      // read the grid rows and row clues
      val grid = Array.fill(height, width)(Block()) // fill the grid with default block instances
      val rowClues = List.newBuilder[Int]

      for (rowIdx <- 0 until height) {
        val rowLine = lines(i).trim.split(" ")
        for (colIdx <- 0 until width) {
          val char = rowLine(colIdx).charAt(0)
          val block = char match {
            case '═' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(1),
              Direction.Right -> Some(1),
              Direction.Up -> Some(0),
              Direction.Down -> Some(0)
            ))
            case '║' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(0),
              Direction.Right -> Some(0),
              Direction.Up -> Some(1),
              Direction.Down -> Some(1)
            ))
            case '╔' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(0),
              Direction.Right -> Some(1),
              Direction.Up -> Some(1),
              Direction.Down -> Some(0)
            ))
            case '╗' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(1),
              Direction.Right -> Some(0),
              Direction.Up -> Some(1),
              Direction.Down -> Some(0)
            ))
            case '╚' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(0),
              Direction.Right -> Some(1),
              Direction.Up -> Some(0),
              Direction.Down -> Some(1)
            ))
            case '╝' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(1),
              Direction.Right -> Some(0),
              Direction.Up -> Some(0),
              Direction.Down -> Some(1)
            ))
            case '1' => Block(state = Some(1), paths = Map(
              Direction.Left -> None,
              Direction.Right -> None,
              Direction.Up -> None,
              Direction.Down -> None
            ))
            case '0' => Block(state = Some(0), paths = Map(
              Direction.Left -> None,
              Direction.Right -> None,
              Direction.Up -> None,
              Direction.Down -> None
            ))
            case '_' => Block(state = None, paths = Map(
              Direction.Left -> None,
              Direction.Right -> None,
              Direction.Up -> None,
              Direction.Down -> None
            ))
            case _ => throw new IllegalArgumentException(s"Invalid character in puzzle: $char")
          }
          grid(rowIdx)(colIdx) = block
        }
        rowClues += rowLine.last.toInt
        i += 1
      }

      // construct the puzzle and add it to the list
      puzzles = Puzzle((width, height), grid, rowClues.result(), columnClues) :: puzzles
    }

    puzzles.reverse
  }

  def writeSolution(filename: String, solutions: List[Solution]): Unit = {
    val writer = new PrintWriter(filename)
    solutions.foreach { solution =>
      writer.println(solution.toString)
      writer.println() // place a blank line between solutions
    }
    writer.close()
  }
}