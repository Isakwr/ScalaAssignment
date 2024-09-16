// PuzzleReaderWriter.scala

import io.Source
import java.io.PrintWriter

object PuzzleReaderWriter {

  def readPuzzles(filename: String): List[Puzzle] = {
    // read from the file and translate to the Puzzle structure
    val source = Source.fromFile(filename, "UTF-8")

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
            ), possibleTracks = Set(Horizontal)
            )
            case '║' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(0),
              Direction.Right -> Some(0),
              Direction.Up -> Some(1),
              Direction.Down -> Some(1)
            ), possibleTracks = Set(Vertical)
            )
            case '╔' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(0),
              Direction.Right -> Some(1),
              Direction.Up -> Some(1),
              Direction.Down -> Some(0)
            ), possibleTracks = Set(CurveDownRight)
            )
            case '╗' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(1),
              Direction.Right -> Some(0),
              Direction.Up -> Some(1),
              Direction.Down -> Some(0)
            ), possibleTracks = Set(CurveDownLeft)
            )
            case '╚' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(0),
              Direction.Right -> Some(1),
              Direction.Up -> Some(0),
              Direction.Down -> Some(1)
            ), possibleTracks = Set(CurveUpRight)
            )
            case '╝' => Block(state = Some(1), paths = Map(
              Direction.Left -> Some(1),
              Direction.Right -> Some(0),
              Direction.Up -> Some(0),
              Direction.Down -> Some(1)
            ), possibleTracks = Set(CurveUpLeft)
            )
            case '1' => Block(state = Some(1), paths = Map(
              Direction.Left -> None,
              Direction.Right -> None,
              Direction.Up -> None,
              Direction.Down -> None
            ), possibleTracks = Set(Horizontal, Vertical, CurveDownRight, CurveDownLeft, CurveUpRight, CurveUpLeft)
            )
            case '0' => Block(state = Some(0), paths = Map(
              Direction.Left -> None,
              Direction.Right -> None,
              Direction.Up -> None,
              Direction.Down -> None
            ), possibleTracks = Set.empty)
            case '_' => Block(state = None, paths = Map(
              Direction.Left -> None,
              Direction.Right -> None,
              Direction.Up -> None,
              Direction.Down -> None
            ), possibleTracks = Set(Horizontal, Vertical, CurveDownRight, CurveDownLeft, CurveUpRight, CurveUpLeft)
            )
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

def markNonTracksRows(puzzle: Puzzle): Puzzle = {
  val newGrid = puzzle.grid.map(_.clone())

  for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
    val count = row.count(_.state.contains(1))
    if (count == puzzle.rowClues(rowIndex)) {
      for ((block, colIndex) <- row.zipWithIndex) {
        if (!block.state.contains(1)) {
          newGrid(rowIndex)(colIndex) = block.copy(state = Some(0))
        }
      }
    }
  }
  puzzle.copy(grid = newGrid)
}

def markNonTracksColumns(puzzle: Puzzle): Puzzle = {
  val newGrid = puzzle.grid.map(_.clone())
  val columnCounts = Array.fill(puzzle.size._1)(0)

  for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
    for ((block, colIndex) <- row.zipWithIndex) {
      if (block.state.contains(1)) {
        columnCounts(colIndex) += 1
      }
    }
  }

  for (colIndex <- columnCounts.indices) {
    if (columnCounts(colIndex) == puzzle.columnClues(colIndex)) {
      for (rowIndex <- puzzle.grid.indices) {
        val block = newGrid(rowIndex)(colIndex)
        if (!block.state.contains(1)) {
          newGrid(rowIndex)(colIndex) = block.copy(state = Some(0))
        }
      }
    }
  }
  puzzle.copy(grid = newGrid)
}

def updateBlockPaths(block: Block): Block = {
  if (block.possibleTracks.size == 1) {
    val trackType = block.possibleTracks.head
    val newPaths = Direction.values.map { dir =>
      dir -> Some(if (trackType.connects.contains(dir)) 1 else 0)
    }.toMap
    block.copy(state = Some(1), paths = newPaths)
  } else {
    block
  }
}