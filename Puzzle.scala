import PuzzleChecker.{completeColumn, completeRow, connect, extendParts, fillCorner, markNonTracksColumns, markNonTracksRows}
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
    // Update only the specific direction, keep all others unchanged
    val updatedPaths = paths.updated(direction, Some(pathExists))
    copy(paths = updatedPaths)
  }

  def isFullyKnown: Boolean = state.isDefined && paths.values.forall(_.isDefined)

  override def toString: String = {
    state match {
      case Some(1) =>
        // Determine the path configuration
        val isLeft = paths(Direction.Left).contains(1)
        val isRight = paths(Direction.Right).contains(1)
        val isUp = paths(Direction.Up).contains(1)
        val isDown = paths(Direction.Down).contains(1)

        // Return the appropriate symbol based on the paths
        (isLeft, isRight, isUp, isDown) match {
          case (true, true, false, false) => "═" // Left and Right
          case (false, false, true, true) => "║" // Up and Down
          case (false, true, false, true) => "╔" // Right and Down
          case (true, false, false, true) => "╗" // Left and Down
          case (false, true, true, false) => "╚" // Up and Right
          case (true, false, true, false) => "╝" // Left and Up
          case _ => "?" // Unknown or invalid configuration
        }
      case Some(0) => "0" // No track
      case None => "_"   // Unknown state
    }
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
                 )

// companion object to provide utility methods related to Puzzle
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
    
    for (rowIdx <- newGrid(colIndex).indices) {
      newGrid(rowIdx)(colIndex) = newGrid(rowIdx)(colIndex).updatedBlockState(1)
    }

    puzzle.copy(grid = newGrid)
  }
  
  def solve(puzzle: Puzzle): Solution = {
    var updatedPuzzle = puzzle



    // fill full rows with track pieces
    for (rowIndex <- puzzle.grid.indices) {
      if (PuzzleChecker.isFullRow(puzzle, rowIndex)) {
        println(s"Filling row $rowIndex with track pieces")
        updatedPuzzle = fillFullRow(updatedPuzzle, rowIndex)
      }
    }

    // fill full columns with track pieces
    for (colIndex <- puzzle.grid.head.indices) {
      if (PuzzleChecker.isFullColumn(puzzle, colIndex)) {
        println(s"Filling column $colIndex with track pieces")
        updatedPuzzle = fillFullColumn(updatedPuzzle, colIndex)
      }
    }
    updatedPuzzle = markNonTracksRows(updatedPuzzle)
    updatedPuzzle = markNonTracksColumns(updatedPuzzle)



    for(i <- 0 until 30){
      updatedPuzzle = markNonTracksRows(updatedPuzzle)
      updatedPuzzle = markNonTracksColumns(updatedPuzzle)
      updatedPuzzle = extendParts(updatedPuzzle)
      updatedPuzzle = markNonTracksRows(updatedPuzzle)
      updatedPuzzle = markNonTracksColumns(updatedPuzzle)
      updatedPuzzle = completeRow(updatedPuzzle)
      updatedPuzzle = completeColumn(updatedPuzzle)
      updatedPuzzle = fillCorner(updatedPuzzle)
    }

    
    updatedPuzzle = connect(updatedPuzzle)
    updatedPuzzle = connect(updatedPuzzle)
    updatedPuzzle = connect(updatedPuzzle)


    println(updatedPuzzle.grid(0)(0).paths)

    // remaining solving logic

    // create Solution object based on the updated puzzle grid
    val solvedGrid = updatedPuzzle.grid.map(_.map(_.toString.charAt(0)))
    solvedGrid.foreach(row => println(row.mkString(" ")))
    Solution(solvedGrid)
  }
  
}

// solution case class to represent a Solution to a Puzzle
case class Solution(grid: Array[Array[Char]]) {
  // convert the grid to a string for output
  override def toString: String =
    grid.map(_.mkString(" ")).mkString("\n")
}