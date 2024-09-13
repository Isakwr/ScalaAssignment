
// PuzzleChecker.scala

object PuzzleChecker {

  // Method to check if any row is fully filled according to the row clues
  def isFullRow(puzzle: Puzzle, rowIndex: Int): Boolean = {
    val clue = puzzle.rowClues(rowIndex)
    val width = puzzle.size._1
    width == clue // check if the width matches the clue
  }

  def completeRow(puzzle: Puzzle): Puzzle = {
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      val noBlockrow: Int = row.count(block => {
        block.state.contains(0)
      })
      if (puzzle.size._1 - noBlockrow == puzzle.rowClues(rowIndex)) {
        for (block <- row) {
          if (!block.state.contains(0)) {
            block.state = Some(1)
          }
        }
      }
    }
    puzzle
  }

  def extendParts(puzzle: Puzzle): Puzzle = {
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex) {
        // If the block is a track with a path
        val pathCount = block.paths.values.count {
          case Some(1) => true
          case _       => false
        }
        if (block.state.contains(1) && pathCount == 2) {
          // Place block at each path direction
          if (block.paths(Direction.Left).contains(1)) {
            // Check if neighbor is in bounds, if true, give them state = Some(1)
            if (inBounds(rowIndex, colIndex - 1, puzzle.size._1, puzzle.size._2)) {
              puzzle.grid(rowIndex)(colIndex - 1).state = Some(1)
            }
          }
          if (block.paths(Direction.Up).contains(1)) {
            if (inBounds(rowIndex - 1, colIndex, puzzle.size._1, puzzle.size._2)) {
              puzzle.grid(rowIndex - 1)(colIndex).state = Some(1)
            }
          }
          if (block.paths(Direction.Right).contains(1)) {
            if (inBounds(rowIndex, colIndex + 1, puzzle.size._1, puzzle.size._2)) {
              puzzle.grid(rowIndex)(colIndex + 1).state = Some(1)
            }
          }
          if (block.paths(Direction.Down).contains(1)) {
            if (inBounds(rowIndex + 1, colIndex, puzzle.size._1, puzzle.size._2)) {
              puzzle.grid(rowIndex + 1)(colIndex).state = Some(1)
            }
          }
        }
      }
    }
    puzzle
  }

  // Checks if block (row index, column index) is in bound of the matrix
  // Needs row, col (position of block) and rows cols (size of puzzle)
  def inBounds(row: Int, col: Int, rows: Int, cols: Int): Boolean = {
    row >= 0 && row < rows && col >= 0 && col < cols
  }

  def markNonTracksRows(puzzle: Puzzle): Puzzle = {
    var loop: Int = 0
    puzzle.grid.foreach((row) => {
      var count: Int = row.count((blk) => {
        blk.state.contains(1)
      })
      if (count == puzzle.rowClues(loop)) {
        row.foreach((element) => {
          if (!element.state.contains(1)) {
            element.state = Some(0)
          }
        })
      }
      loop += 1
    })
    puzzle
  }

  def markNonTracksColumns(puzzle: Puzzle): Puzzle = {
    var loop: Int = 0
    var check = new Array[Int](puzzle.size._2)
    puzzle.grid.foreach((row) => {
      for ((elem, i) <- row.zipWithIndex) {
        if (elem.state.contains(1)) {
          check(i) += 1
        }
      }
    })
    for ((elem, i) <- check.zipWithIndex) {
      if (elem >= puzzle.columnClues(i)) {
        puzzle.grid.foreach((row) => {
          if (!row(i).state.contains(1)) {
            row(i).state = Some(0)
          }
        })
      }
    }
    puzzle
  }

  // Check if block is on the boundary of the puzzle
  // Needs block, row and column index of said block, and puzzle
  def isOnBoundary(block: Block, rowIndex: Int, colIndex: Int, puzzle: Puzzle): Boolean = {
    rowIndex == 0 || colIndex == 0 || rowIndex == puzzle.size._1 - 1 || colIndex == puzzle.size._2 - 1
  }

  // Method to check if any column is fully filled according to the column clues
  def isFullColumn(puzzle: Puzzle, colIndex: Int): Boolean = {
    val clue = puzzle.columnClues(colIndex)
    val height = puzzle.size._2
    height == clue // check if the number of filled blocks matches the clue
  }
}

