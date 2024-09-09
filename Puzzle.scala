import PuzzleChecker.{extendParts, markNonTracksColumns, markNonTracksRows}
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
                 columnClues: List[Int]     // clues for each column
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

    updatedPuzzle = extendParts(updatedPuzzle)
    //updatedPuzzle = markNonTracksRows(updatedPuzzle)
    //updatedPuzzle = markNonTracksColumns(updatedPuzzle)



    // remaining solving logic

    // create Solution object based on the updated puzzle grid
    val solvedGrid = updatedPuzzle.grid.map(_.map(_.state.getOrElse(0).toString.charAt(0)))
    Solution(solvedGrid)
  }
  
}

// solution case class to represent a Solution to a Puzzle
case class Solution(grid: Array[Array[Char]]) {
  // convert the grid to a string for output
  override def toString: String =
    grid.map(_.mkString(" ")).mkString("\n")
}