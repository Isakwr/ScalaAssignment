import scala.collection.mutable.Set
import PuzzleChecker.{completeColumn, completeRow, connect, extendParts, fillCorner, finishConnect, loopChecker, makeFullyKnown, markNonTracksColumns, markNonTracksRows}

import java.io.PrintWriter
import scala.collection.mutable

object Direction extends Enumeration {
  val Left, Up, Right, Down = Value
}

case class Block (
                 var state: Option[Int], //none for unkown, Some(1) for track, Some(0) for no track
                 var paths: Map[Direction.Value, Option[Int]] //map of paths with binary representation
                 ) {
  def updatedBlockState(trackExists: Int): Block = {
    copy(state = Some(trackExists))
  }

  def updatePath(direction: Direction.Value, pathExists: Int): Block = {
    //update only the specific direction, keep all others unchanged
    val updatedPaths = paths.updated(direction, Some(pathExists))
    copy(paths = updatedPaths)
  }

  def isFullyKnown: Boolean = state.isDefined && paths.values.forall(_.isDefined)

  override def toString: String = {
    state match {
      case Some(1) =>
        //determine the path configuration
        val isLeft = paths(Direction.Left).contains(1)
        val isRight = paths(Direction.Right).contains(1)
        val isUp = paths(Direction.Up).contains(1)
        val isDown = paths(Direction.Down).contains(1)

        //return the appropriate symbol based on the paths
        (isLeft, isRight, isUp, isDown) match {
          case (true, true, false, false) => "═" // Left and Right
          case (false, false, true, true) => "║" // Up and Down
          case (false, true, false, true) => "╔" // Right and Down
          case (true, false, false, true) => "╗" // Left and Down
          case (false, true, true, false) => "╚" // Up and Right
          case (true, false, true, false) => "╝" // Left and Up
          case _ => " " // Unknown or invalid configuration
        }
      case Some(0) => " " // No track
      case None => " "   // Unknown state
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
    val newGrid = puzzle.grid.map(_.clone()) 

    for (colIdx <- newGrid(rowIndex).indices) {
      newGrid(rowIndex)(colIdx) = newGrid(rowIndex)(colIdx).updatedBlockState(1)
    }

    puzzle.copy(grid = newGrid)
  }

  def fillFullColumn(puzzle: Puzzle, colIndex: Int): Puzzle = {
    val newGrid = puzzle.grid.map(_.clone()) 

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
    var done: Boolean = false
    while(!done){
      var copyPuzzle = updatedPuzzle

      updatedPuzzle = markNonTracksRows(updatedPuzzle)
      updatedPuzzle = markNonTracksColumns(updatedPuzzle)
      updatedPuzzle = extendParts(updatedPuzzle)
      updatedPuzzle = markNonTracksRows(updatedPuzzle)
      updatedPuzzle = markNonTracksColumns(updatedPuzzle)
      updatedPuzzle = completeRow(updatedPuzzle)
      updatedPuzzle = completeColumn(updatedPuzzle)
      updatedPuzzle = fillCorner(updatedPuzzle)

      if(copyPuzzle.grid sameElements updatedPuzzle.grid) {
        for(i <- 0 until 30) {
          updatedPuzzle = markNonTracksRows(updatedPuzzle)
          updatedPuzzle = markNonTracksColumns(updatedPuzzle)
          updatedPuzzle = extendParts(updatedPuzzle)
          updatedPuzzle = markNonTracksRows(updatedPuzzle)
          updatedPuzzle = markNonTracksColumns(updatedPuzzle)
          updatedPuzzle = completeRow(updatedPuzzle)
          updatedPuzzle = completeColumn(updatedPuzzle)
          updatedPuzzle = fillCorner(updatedPuzzle)
        }
        done = true
      }
    }

    var trackPieceNum = 0
    for (row <- puzzle.grid) {
      for (block <- row) {
        if (block.state.contains(1)) {
          trackPieceNum += 1
        }
      }
    }

    for(i <- 0 until 50) {
      updatedPuzzle = connect(updatedPuzzle)
      updatedPuzzle = makeFullyKnown(updatedPuzzle)
      updatedPuzzle = connect(updatedPuzzle)
      updatedPuzzle = makeFullyKnown(updatedPuzzle)
      updatedPuzzle = connect(updatedPuzzle)
      updatedPuzzle = finishConnect(updatedPuzzle)
      updatedPuzzle = makeFullyKnown(updatedPuzzle)
      updatedPuzzle = connect(updatedPuzzle)
    }



    val solvedGrid: Array[Array[Char]] = updatedPuzzle.grid.map(_.map {
      case block: Block if block.state.contains(1) => block.toString.charAt(0)
      case Block(Some(0), _) => ' '
      case _ => '_'
    })


    //return the solution
    Solution(solvedGrid, updatedPuzzle.rowClues, updatedPuzzle.columnClues)
  }

  
  case class Solution(grid: Array[Array[Char]], rowClues: List[Int], columnClues: List[Int]) {
    override def toString: String = {
      
      val colCluesString = columnClues.mkString(" ")
      val gridWithRowClues = grid.zip(rowClues).map { case (row, clue) =>
        row.mkString(" ") + " " + clue.toString
      }.mkString("\n")
      
      colCluesString + "\n" + gridWithRowClues
    }
  }
}