import PuzzleChecker.{completeRow, extendParts, markNonTracksColumns, markNonTracksRows}

// Puzzle.scala

object Direction extends Enumeration {
  val Left, Up, Right, Down = Value
}

case class Block (
                   var state: Option[Int], // None for unkown, Some(1) for track, Some(0) for no track
                   var paths: Map[Direction.Value, Option[Int]] // map of paths with binary representation
                 ) {
  def updatedBlockState(trackExists: Int): Block = {
    copy(state = Some(trackExists))
  }

  def updatePath(direction: Direction.Value, pathExists: Int): Block = {
    copy(paths = paths.updated(direction, Some(pathExists)))
  }

  def isFullyKnown: Boolean = state.isDefined && paths.values.forall(_.isDefined)

  def isTrack: Boolean = state.contains(1)

  def isEmpty: Boolean = state.isEmpty

  override def toString: String = {
    val blockState = state.map {
      case 1 => "1"
      case 0 => "0"
      case _ => "?"
    }.getOrElse("?")

    val pathState = paths.map {
      case (Direction.Left, Some(1)) => "<-"
      case (Direction.Right, Some(1)) => "->"
      case (Direction.Up, Some(1)) => "^"
      case (Direction.Down, Some(1)) => "v"
      case _ => "_"
    }.mkString(" ")

    s"Block: $blockState, Paths: $pathState"
  }
}

object Block {
  def apply(): Block = new Block(
    state = None,
    paths = Map(
      Direction.Left -> None,
      Direction.Up -> None,
      Direction.Right -> None,
      Direction.Down -> None
    )
  )
}

case class Puzzle(
                   size: (Int, Int),         // (width, height) of the puzzle
                   grid: Array[Array[Block]],  // 2D array representing the grid
                   rowClues: List[Int],        // clues for each row
                   columnClues: List[Int],     // clues for each column
                 ) {
  def deepCopy(): Puzzle = {
    val newGrid = grid.map(_.map(_.copy()))
    copy(grid = newGrid)
  }

  def isInBounds(row: Int, col: Int): Boolean = {
    row >= 0 && row < size._2 && col >= 0 && col < size._1
  }

  def getNeighbors(row: Int, col: Int): Map[Direction.Value, (Int, Int)] = {
    Map(
      Direction.Left -> (row, col - 1),
      Direction.Up -> (row - 1, col),
      Direction.Right -> (row, col + 1),
      Direction.Down -> (row + 1, col)
    ).filter { case (_, (r, c)) => isInBounds(r, c) }
  }
}

object Puzzle {

  def fillFullRow(puzzle: Puzzle, rowIndex: Int): Puzzle = {
    val newGrid = puzzle.grid.map(_.clone()) // create a copy of the grid to modify

    for (colIdx <- newGrid(rowIndex).indices) {
      newGrid(rowIndex)(colIdx) = newGrid(rowIndex)(colIdx).updatedBlockState(1)
    }

    puzzle.copy(grid = newGrid)
  }

  def fillFullColumn(puzzle: Puzzle, colIndex: Int): Puzzle = {
    val newGrid = puzzle.grid.map(_.clone()) // create a copy of the grid to modify

    for (rowIdx <- newGrid.indices) {
      newGrid(rowIdx)(colIndex) = newGrid(rowIdx)(colIndex).updatedBlockState(1)
    }

    puzzle.copy(grid = newGrid)
  }

  def isSolved(puzzle: Puzzle): Boolean = {
    puzzle.grid.flatten.forall(_.state.isDefined) &&
      puzzle.rowClues.zipWithIndex.forall { case (clue, rowIdx) =>
        puzzle.grid(rowIdx).count(_.state.contains(1)) == clue
      } &&
      puzzle.columnClues.zipWithIndex.forall { case (clue, colIdx) =>
        puzzle.grid.map(_(colIdx)).count(_.state.contains(1)) == clue
      }
  }

  def solveWithBacktracking(puzzle: Puzzle, rowIndex: Int = 0, colIndex: Int = 0): Option[Puzzle] = {
    // Base case: if puzzle is solved, return it
    if (isSolved(puzzle)) return Some(puzzle)

    // Check if we have reached the end of the grid
    if (rowIndex >= puzzle.size._2) return None

    // Move to the next block (or next row)
    val (nextRow, nextCol) = if (colIndex == puzzle.size._1 - 1) (rowIndex + 1, 0) else (rowIndex, colIndex + 1)

    // If the current block is already determined, skip to the next one
    if (puzzle.grid(rowIndex)(colIndex).isFullyKnown) return solveWithBacktracking(puzzle, nextRow, nextCol)

    // Try placing a track if it connects to an adjacent track or is an endpoint
    if (canPlaceTrack(puzzle, rowIndex, colIndex)) {
      val newPuzzle = puzzle.deepCopy()
      newPuzzle.grid(rowIndex)(colIndex).state = Some(1)

      // Ensure paths are connected
      connectPaths(newPuzzle, rowIndex, colIndex)

      // Recursively solve the next block
      val result = solveWithBacktracking(newPuzzle, nextRow, nextCol)
      if (result.isDefined) return result
    }

    // Also try placing a non-track
    val newPuzzle = puzzle.deepCopy()
    newPuzzle.grid(rowIndex)(colIndex).state = Some(0)

    val result = solveWithBacktracking(newPuzzle, nextRow, nextCol)
    if (result.isDefined) return result

    // If no valid moves, backtrack
    None
  }

  def canPlaceTrack(puzzle: Puzzle, rowIndex: Int, colIndex: Int): Boolean = {
    val neighbors = puzzle.getNeighbors(rowIndex, colIndex)
    neighbors.exists { case (_, (r, c)) =>
      puzzle.grid(r)(c).isTrack
    }
  }

  def connectPaths(puzzle: Puzzle, rowIndex: Int, colIndex: Int): Unit = {
    val neighbors = puzzle.getNeighbors(rowIndex, colIndex)
    for ((direction, (r, c)) <- neighbors) {
      if (puzzle.grid(r)(c).isTrack) {
        puzzle.grid(rowIndex)(colIndex) = puzzle.grid(rowIndex)(colIndex).updatePath(direction, 1)
        puzzle.grid(r)(c) = puzzle.grid(r)(c).updatePath(oppositeDirection(direction), 1)
      }
    }
  }

  def oppositeDirection(direction: Direction.Value): Direction.Value = {
    direction match {
      case Direction.Left => Direction.Right
      case Direction.Right => Direction.Left
      case Direction.Up => Direction.Down
      case Direction.Down => Direction.Up
    }
  }

  def solve(puzzle: Puzzle): Solution = {
    var updatedPuzzle = puzzle

    // Apply initial simple rules
    for (rowIndex <- puzzle.grid.indices) {
      if (PuzzleChecker.isFullRow(puzzle, rowIndex)) {
        updatedPuzzle = fillFullRow(updatedPuzzle, rowIndex)
      }
    }

    for (colIndex <- puzzle.grid.head.indices) {
      if (PuzzleChecker.isFullColumn(puzzle, colIndex)) {
        updatedPuzzle = fillFullColumn(updatedPuzzle, colIndex)
      }
    }

    updatedPuzzle = markNonTracksRows(updatedPuzzle)
    updatedPuzzle = markNonTracksColumns(updatedPuzzle)

    // Further refining using additional methods
    for(i <- 0 until 15){
      updatedPuzzle = markNonTracksRows(updatedPuzzle)
      updatedPuzzle = markNonTracksColumns(updatedPuzzle)
      updatedPuzzle = extendParts(updatedPuzzle)
      updatedPuzzle = markNonTracksRows(updatedPuzzle)
      updatedPuzzle = markNonTracksColumns(updatedPuzzle)
      updatedPuzzle = completeRow(updatedPuzzle)
    }

    // If still unsolved, use backtracking
    solveWithBacktracking(updatedPuzzle) match {
      case Some(solvedPuzzle) =>
        val solvedGrid = solvedPuzzle.grid.map(_.map(_.state.getOrElse(0).toString.charAt(0)))
        Solution(solvedGrid)
      case None =>
        throw new RuntimeException("Puzzle cannot be solved.")
    }
  }

}

// solution case class to represent a Solution to a Puzzle
case class Solution(grid: Array[Array[Char]]) {
  // convert the grid to a string for output
  override def toString: String =
    grid.map(_.mkString(" ")).mkString("\n")
}
