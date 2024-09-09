// PuzzleChecker.scala

object PuzzleChecker {

  // method to check if any row is fully filled according to the row clues
  def isFullRow(puzzle: Puzzle, rowIndex: Int): Boolean = {
    val clue = puzzle.rowClues(rowIndex)
    val width = puzzle.size._1
    width == clue // check if the width matches the clue
  }

  
  
  def extendParts(puzzle: Puzzle): Puzzle = {
    for((row, rowIndex) <- puzzle.grid.zipWithIndex){
      for((block, colIndex) <- row.zipWithIndex){
        val neighbours = List(
          (rowIndex - 1, colIndex), // Up
          (rowIndex + 1, colIndex), // Down
          (rowIndex, colIndex - 1), // Left
          (rowIndex, colIndex + 1)  // Right
        )
        for(adj <- neighbours){
          val neighbourblock = puzzle.grid(adj._1)(adj._2)
          if(!inBounds(adj._1, adj._2, puzzle.size._1, puzzle.size._2)){
            if(adj._1 == rowIndex - 1){
              if(neighbourblock.paths(Direction.Down).contains(1)) {
                block.state = Some(1)
                block.paths = Map(
                  Direction.Left -> None,
                  Direction.Up -> Some(1),
                  Direction.Right -> None,
                  Direction.Down -> None
                )
              }
            }
            if (adj._2 == colIndex - 1) {
              if(neighbourblock.paths(Direction.Right).contains(1)){
                block.state = Some(1)
                block.paths = Map(
                  Direction.Left -> Some(1),
                  Direction.Up -> None,
                  Direction.Right -> None,
                  Direction.Down -> None
                )
              }

            }
            if (adj._1 == rowIndex + 1) {
              if(neighbourblock.paths(Direction.Up).contains(1)){
                block.state = Some(1)
                block.paths = Map(
                  Direction.Left -> None,
                  Direction.Up -> None,
                  Direction.Right -> None,
                  Direction.Down -> Some(1)
                )
              }

            }
            if (adj._2 == colIndex + 1) {
              if(neighbourblock.paths(Direction.Left).contains(1)){
                block.state = Some(1)
                block.paths = Map(
                  Direction.Left -> None,
                  Direction.Up -> None,
                  Direction.Right -> Some(1),
                  Direction.Down -> None
                )
              }

            }
          }
        }
      }
    }
    puzzle
  }

  //checks if block (row index, column index) is in bound of the matrix
  // needs row, col (position of block) and rows cols (size of puzzle)
  def inBounds(row: Int, col: Int, rows: Int, cols: Int): Boolean = {
    row >= 0 && row < rows && col >= 0 && col < cols
  }

  def markNonTracksRows(puzzle: Puzzle): Puzzle = {


    var loop: Int = 0
    puzzle.grid.foreach((row) => {
      var count: Int = row.count((blk) => {
        blk.state.contains(1)
      })
      if(count == puzzle.rowClues(loop)){
        row.foreach((element) => {

          if(!element.state.contains(1)){
            element.state = Some('N')
          }
        })
      }
      loop += 1

    })
    return puzzle
  }

  def markNonTracksColumns(puzzle: Puzzle): Puzzle = {
    var loop: Int = 0
    var check = new Array[Int](puzzle.size._2)
    puzzle.grid.foreach((row) => {
      var j: Int = 0
      for ((elem, i) <- row.zipWithIndex) {
        if(elem.state.contains(1)){
          check(i) += 1
        }
      }
    })
    for ((elem, i) <- check.zipWithIndex) {
      if(elem >= puzzle.columnClues(i)){
        puzzle.grid.foreach((row) => {
          if(!row(i).state.contains(1)){
            row(i).state = Some('N')
          }
        })
      }
    }
    return puzzle
  }


  // check if block is on boundary of puzzle
  //needs block, row and column index of said block, and puzzle
  def isOnBoundary(block: Block, rowIndex: Int, colIndex: Int, puzzle: Puzzle): Boolean = {
    var answer: Boolean = false
    if(rowIndex == 0 || colIndex == 0 || rowIndex == puzzle.size._1 -1
      || colIndex == puzzle.size._2 - 1) {
      return true
    }
    else
      return false
  }

  // method to check if any column is fully filled according to the column clues
  def isFullColumn(puzzle: Puzzle, colIndex: Int): Boolean = {
    val clue = puzzle.columnClues(colIndex)
    val height = puzzle.size._2
    height == clue // check if the number of filled blocks matches the clue
  }
}
