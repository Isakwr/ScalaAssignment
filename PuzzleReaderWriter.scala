import java.io.PrintWriter
import scala.io.Source

object PuzzleReaderWriter {

  def readPuzzles(filename: String): List[Puzzle] = {
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    var puzzles = List.empty[Puzzle]
    var i = 0

    // first line indicates the number of puzzles
    val numPuzzles = lines(i).split(" ")(1).toInt
    i += 1

    // parse each puzzle
    while (i < lines.length) {
      // read the size of the puzzle (e.g., "size 4x4")
      val sizeLine = lines(i).split(" ")(1).split("x")
      val (width, height) = (sizeLine(0).toInt, sizeLine(1).toInt)
      i += 1

      // read column clues
      val columnClues = lines(i).trim.split(" ").map(_.toInt).toList
      i += 1

      // read the grid rows and row clues
      val grid = Array.fill(height, width)(Block())
      val rowClues = List.newBuilder[Int]

      for (rowIdx <- 0 until height) {
        val rowLine = lines(i).trim.split(" ")
        rowClues += rowLine.last.toInt

        for (colIdx <- 0 until width) {
          grid(rowIdx)(colIdx) = rowLine(colIdx).charAt(0) match {
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
              Direction.Up -> Some(0),
              Direction.Down -> Some(1)
            ))
            case '╗' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(1),
              Direction.Right -> Some(0),
              Direction.Up -> Some(0),
              Direction.Down -> Some(1)
            ))
            case '╚' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(0),
              Direction.Right -> Some(1),
              Direction.Up -> Some(1),
              Direction.Down -> Some(0)
            ))
            case '╝' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(1),
              Direction.Right -> Some(0),
              Direction.Up -> Some(1),
              Direction.Down -> Some(0)
            ))
            case '_' => Block(state = None, paths = Map(
              Direction.Left -> None,
              Direction.Right -> None,
              Direction.Up -> None,
              Direction.Down -> None
            ))
            case _ => throw new IllegalArgumentException(s"Invalid character in puzzle: ${rowLine(colIdx)}")
          }
        }
        i += 1
      }

      puzzles = Puzzle((width, height), grid, rowClues.result(), columnClues) :: puzzles
    }

    puzzles.reverse
  }

  def writeSolution(filename: String, solutions: List[Solution]): Unit = {
    val writer = new PrintWriter(filename)
    writer.println(s"puzzles ${solutions.size}")

    for (solution <- solutions) {
      writer.println(s"size ${solution.grid.head.length}x${solution.grid.length}")
      writer.println(solution.grid.map(_.mkString(" ")).mkString("\n"))
    }

    writer.close()
  }
}
