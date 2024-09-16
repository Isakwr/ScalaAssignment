// Puzzle.scala

object Direction extends Enumeration {
  val Left, Up, Right, Down = Value
}

// define the TrackType trait and its case objects, for directional track pieces
sealed trait TrackType {
  def symbol: Char
  def connects: Set[Direction.Value]
}

case object Horizontal extends TrackType {
  val symbol = '═'
  val connects = Set(Direction.Left, Direction.Right)
}

case object Vertical extends TrackType {
  val symbol = '║'
  val connects = Set(Direction.Up, Direction.Down)
}

case object CurveDownRight extends TrackType {
  val symbol = '╔'
  val connects = Set(Direction.Down, Direction.Right)
}

case object CurveDownLeft extends TrackType {
  val symbol = '╗'
  val connects = Set(Direction.Down, Direction.Left)
}

case object CurveUpRight extends TrackType {
  val symbol = '╚'
  val connects = Set(Direction.Up, Direction.Right)
}

case object CurveUpLeft extends TrackType {
  val symbol = '╝'
  val connects = Set(Direction.Up, Direction.Left)
}

case class Block (
                 state: Option[Int], // none for unknown, Some(1) for track, Some(0) for no track
                 paths: Map[Direction.Value, Option[Int]], // map of paths with binary representation
                 possibleTracks: Set[TrackType]
                 ) {
  // this method will modify the state of a single block without modifying `paths` or `possibleTracks`
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
    ),
    possibleTracks = Set(
      Horizontal, Vertical, CurveDownRight, CurveDownLeft, CurveUpRight,
      CurveUpLeft
    )
  )
}

case class Puzzle(
                 size: (Int, Int),         // (width, height) of the puzzle
                 grid: Array[Array[Block]],  // 2D array representing the grid
                 rowClues: List[Int],       // clues for each row
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
    
    for (rowIdx <- newGrid.indices) {
      newGrid(rowIdx)(colIndex) = newGrid(rowIdx)(colIndex).updatedBlockState(1)
    }

    puzzle.copy(grid = newGrid)
  }

  def fillFullRowsAndColumns(puzzle: Puzzle): Puzzle = {
    var updatedPuzzle = puzzle

    // fill full rows with default track pieces
    for (rowIndex <- puzzle.grid.indices) {
      if (PuzzleChecker.isFullRow(puzzle, rowIndex)) {
        println(s"Filling row $rowIndex with track pieces")
        updatedPuzzle = fillFullRow(updatedPuzzle, rowIndex)
      }
    }
    
    // fill full columns with default track pieces
    for (colIndex <- puzzle.grid.head.indices) {
      if (PuzzleChecker.isFullColumn(puzzle, colIndex)) {
        println(s"Filling column $colIndex with track pieces")
        updatedPuzzle = fillFullColumn(updatedPuzzle, colIndex)
      }
    }
    
    updatedPuzzle
  }
  
  // this method will iterate over the entire puzzle and update multiple block instances based on their `possibleTracks`
  def updateBlockStates(puzzle: Puzzle): Puzzle = {
    val newGrid = puzzle.grid.map(_.clone())
    
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex if block.state.isEmpty) {
        if (block.possibleTracks.size == 1) {
          val trackType = block.possibleTracks.head
          val newPaths = Direction.values.map { dir =>
            dir -> Some(if (trackType.connects.contains(dir)) 1 else 0)
          }.toMap
          newGrid(rowIndex)(colIndex) = block.copy(state = Some(1), paths = newPaths)
        } else if (block.possibleTracks.isEmpty) {
          // if there are no possible tracks, the block must not contain a track
          newGrid(rowIndex)(colIndex) = block.copy(state = Some(0))
        }
      }
    }
    
    puzzle.copy(grid = newGrid)
  }
  
  def solve(puzzle: Puzzle): Solution = {
    var updatedPuzzle = puzzle
    var progress = true
    
    while (progress) {
      val beforeState = updatedPuzzle.grid.map(_.map(_.state))
      
      // apply methods
      updatedPuzzle = fillFullRowsAndColumns(updatedPuzzle)
      updatedPuzzle = markNonTracksRows(updatedPuzzle)
      updatedPuzzle = markNonTracksColumns(updatedPuzzle)
      updatedPuzzle = PuzzleChecker.updatePossibleTracks(updatedPuzzle)
      updatedPuzzle = updateBlockStates(updatedPuzzle)
      updatedPuzzle = PuzzleChecker.extendParts(updatedPuzzle)
      updatedPuzzle = PuzzleChecker.enforceConnectivity(updatedPuzzle)
      
      val afterState = updatedPuzzle.grid.map(_.map(_.state))
      progress = beforeState != afterState
    }
    
    // check connectivity
    if (!PuzzleChecker.isPathConnected(updatedPuzzle)) {
      // handle case where path is not connected (backtracking?)
    }
    
    // create Solution object based on the updated puzzle grid
    val solvedGrid = updatedPuzzle.grid.map(_.map {
      case Block(Some(1), _, possibleTracks) if possibleTracks.size == 1 =>
        possibleTracks.head.symbol
      case Block(Some(1), _, _) =>
        '1' // unable to determine symbol
      case Block(Some(0), _, _) => '0'
      case _                    => '_'
    })
    Solution(solvedGrid)
  }
  
}

// solution case class to represent a Solution to a Puzzle
case class Solution(grid: Array[Array[Char]]) {
  // convert the grid to a string for output
  override def toString: String =
    grid.map(_.mkString(" ")).mkString("\n")
}