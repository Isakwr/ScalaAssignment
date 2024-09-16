// PuzzleChecker.scala

object PuzzleChecker {

  // method to check if any row is fully filled according to the row clues
  def isFullRow(puzzle: Puzzle, rowIndex: Int): Boolean = {
    val clue = puzzle.rowClues(rowIndex)
    val width = puzzle.size._1
    width == clue // check if the width matches the clue
  }

  def printMatrixWithCoordinatesAndState(puzzle: Puzzle): Unit = {
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex) {
        // Get the state value or use '?' if state is None
        val stateValue = block.state.map(_.toString).getOrElse("?")
        // Print the coordinates and state for each block
        print(s"($rowIndex, $colIndex: $stateValue) ")
      }
      // Print a newline after each row
      println()
    }
  }

  def completeRow(puzzle: Puzzle): Puzzle = {
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      val noBlockrow: Int = row.count(block => {
        block.state.contains(0)
      })
      println(s"row: ${rowIndex} has ${noBlockrow} no track blocks")
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

  def fillCorner(puzzle: Puzzle): Puzzle = {
    //loop through rows
    for((row, rowIndex) <- puzzle.grid.zipWithIndex){
      //loop through each block in row
      for((block, columnIndex) <- row.zipWithIndex){

        //check if available block neighbours is equal to 2
        //if a block has 2 neighbours with value Some(0) or Some(1) - with direction not into the block
        if(block.state.contains(1)) {
          if (rowIndex == 0 && columnIndex == 0) {
            puzzle.grid(rowIndex)(columnIndex+1).state = Some(1)
            puzzle.grid(rowIndex+1)(columnIndex).state = Some(1)
          }
          if (rowIndex == 0 && columnIndex == puzzle.size._2-1) {
            puzzle.grid(rowIndex)(columnIndex-1).state = Some(1)
            puzzle.grid(rowIndex+1)(columnIndex).state = Some(1)
          }
          if (rowIndex == puzzle.size._1-1 && columnIndex == 0) {
            puzzle.grid(rowIndex-1)(columnIndex).state = Some(1)
            puzzle.grid(rowIndex)(columnIndex+1).state = Some(1)
          }
          if (rowIndex == puzzle.size._1-1 && columnIndex == puzzle.size._2-1) {
            puzzle.grid(rowIndex-1)(columnIndex).state = Some(1)
            puzzle.grid(rowIndex)(columnIndex-1).state = Some(1)
          }



          val neighbours: Array[(Int, Int)] = Array(
            (rowIndex, columnIndex - 1), //left
            (rowIndex - 1, columnIndex), //Up
            (rowIndex, columnIndex + 1), //Right
            (rowIndex + 1, columnIndex), //Down
          )
          val directed: Boolean = false
          val count: Int = neighbours.count(neigh => {
            if(inBounds(neigh._1, neigh._2, puzzle.size._1, puzzle.size._2)) {
              puzzle.grid(neigh._1)(neigh._2).state.contains(0)
            }else
              false
          })


          //if block is on boundary of matrix
          if(isOnBoundary(block, rowIndex, columnIndex, puzzle)){
            if(count == 1){
              for(neigh <- neighbours){
                if(inBounds(neigh._1, neigh._2, puzzle.size._1, puzzle.size._2)){
                  if(!puzzle.grid(neigh._1)(neigh._2).state.contains(0)){
                    puzzle.grid(neigh._1)(neigh._2).state = Some(1)
                  }
                }
              }
            }
          }
            // else the block will not be on boundary of matrix
          else {
            if(count == 2){
              for (neigh <- neighbours) {
                if (!puzzle.grid(neigh._1)(neigh._2).state.contains(0)) {
                  puzzle.grid(neigh._1)(neigh._2).state = Some(1)
                }

              }
            }
          }
        }
      }
    }
    puzzle
  }


  
  
  
  
  

  def extendParts(puzzle: Puzzle): Puzzle = {
    printMatrixWithCoordinatesAndState(puzzle)
    for((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex) {
        //If the block is a track with a path
        val pathCount = block.paths.values.count{
          case Some(1) => true
          case _ => false
        }
        if(block.state.contains(1) && pathCount == 2){
          //place block at each path direction
          if(block.paths(Direction.Left).contains(1)){
            //check if neighbour is in bounds, if true, give them state = Some(1)
            if(inBounds(rowIndex, colIndex-1, puzzle.size._1, puzzle.size._2)){
              println(s" 0 - block: (${rowIndex},${colIndex}) setting (${rowIndex},${colIndex-1})")
              puzzle.grid(rowIndex)(colIndex-1).state = Some(1)
            }
          }
          if (block.paths(Direction.Up).contains(1)) {
            if (inBounds(rowIndex-1, colIndex, puzzle.size._1, puzzle.size._2)) {
              println(s" 1 - block: (${rowIndex},${colIndex}) setting (${rowIndex-1} ${colIndex})")
              println(puzzle.grid(rowIndex)(colIndex).paths)
              puzzle.grid(rowIndex-1)(colIndex).state = Some(1)
            }
          }
          if (block.paths(Direction.Right).contains(1)) {
            if (inBounds(rowIndex, colIndex + 1, puzzle.size._1, puzzle.size._2)) {
              println(s" 2 - block: (${rowIndex},${colIndex}) setting (${rowIndex},${colIndex+1})")
              puzzle.grid(rowIndex)(colIndex + 1).state = Some(1)
            }
          }
          if (block.paths(Direction.Down).contains(1)) {
            if (inBounds(rowIndex + 1, colIndex, puzzle.size._1, puzzle.size._2)) {
              println(s" 3- block: (${rowIndex},${colIndex}) setting (${rowIndex+1},${colIndex})")
              puzzle.grid(rowIndex + 1)(colIndex).state = Some(1)
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
            element.state = Some(0)
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
            row(i).state = Some(0)
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
