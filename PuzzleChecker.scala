object PuzzleChecker {

  def isFullRow(puzzle: Puzzle, rowIndex: Int): Boolean = {
    puzzle.grid(rowIndex).count(_.state.contains(1)) == puzzle.rowClues(rowIndex)
  }

  def completeRow(puzzle: Puzzle): Puzzle = {
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      val noBlockRow = row.count(_.state.contains(0))
      if (puzzle.size._1 - noBlockRow == puzzle.rowClues(rowIndex)) {
        for (block <- row if block.isEmpty) {
          block.state = Some(1)
        }
      }
    }
    puzzle
  }

  def extendParts(puzzle: Puzzle): Puzzle = {
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex) {
        val pathCount = block.paths.values.count(_.contains(1))
        if (block.isTrack && pathCount == 2) {
          if (block.paths(Direction.Left).contains(1) && inBounds(rowIndex, colIndex - 1, puzzle.size._1, puzzle.size._2)) {
            puzzle.grid(rowIndex)(colIndex - 1).state = Some(1)
          }
          if (block.paths(Direction.Up).contains(1) && inBounds(rowIndex - 1, colIndex, puzzle.size._1, puzzle.size._2)) {
            puzzle.grid(rowIndex - 1)(colIndex).state = Some(1)
          }
          if (block.paths(Direction.Right).contains(1) && inBounds(rowIndex, colIndex + 1, puzzle.size._1, puzzle.size._2)) {
            puzzle.grid(rowIndex)(colIndex + 1).state = Some(1)
          }
          if (block.paths(Direction.Down).contains(1) && inBounds(rowIndex + 1, colIndex, puzzle.size._1, puzzle.size._2)) {
            puzzle.grid(rowIndex + 1)(colIndex).state = Some(1)
          }
        }
      }
    }
    puzzle
  }

  def inBounds(row: Int, col: Int, rows: Int, cols: Int): Boolean = {
    row >= 0 && row < rows && col >= 0 && col < cols
  }

  def markNonTracksRows(puzzle: Puzzle): Puzzle = {
    puzzle.grid.indices.foreach { rowIndex =>
      if (puzzle.grid(rowIndex).count(_.state.contains(1)) == puzzle.rowClues(rowIndex)) {
        puzzle.grid(rowIndex).foreach { block =>
          if (block.isEmpty) block.state = Some(0)
        }
      }
    }
    puzzle
  }

  def markNonTracksColumns(puzzle: Puzzle): Puzzle = {
    val height = puzzle.size._2
    for (colIndex <- puzzle.grid.head.indices) {
      val count = puzzle.grid.map(_(colIndex)).count(_.state.contains(1))
      if (count == puzzle.columnClues(colIndex)) {
        puzzle.grid.foreach { row =>
          if (row(colIndex).isEmpty) row(colIndex).state = Some(0)
        }
      }
    }
    puzzle
  }

  def isFullColumn(puzzle: Puzzle, colIndex: Int): Boolean = {
    puzzle.grid.map(_(colIndex)).count(_.state.contains(1)) == puzzle.columnClues(colIndex)
  }
}
