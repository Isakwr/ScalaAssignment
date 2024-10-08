import PuzzleChecker.inBounds

object PuzzleChecker {

  // method to check if any row is fully filled according to the row clues
  def isFullRow(puzzle: Puzzle, rowIndex: Int): Boolean = {
    val clue = puzzle.rowClues(rowIndex)
    val width = puzzle.size._1
    width == clue // check if the width matches the clue
  }

  def isFullColumn(puzzle: Puzzle, colIndex: Int): Boolean = {
    val clue = puzzle.columnClues(colIndex)
    val height = puzzle.size._2
    height == clue
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

  def completeColumn(puzzle: Puzzle): Puzzle = {
    val numRows = puzzle.size._1
    val numCols = puzzle.size._2


    for (colIndex <- 0 until numCols) {
      var noBlockCol: Int = 0

      //Count the number of blocks in the column that have state Some(0)
      for (rowIndex <- 0 until numRows) {
        if (puzzle.grid(rowIndex)(colIndex).state.contains(0)) {
          noBlockCol += 1
        }
      }

      println(s"Column: ${colIndex} has ${noBlockCol} no track blocks")

      //If the number of confirmed track pieces equals the column clue, complete the column
      if (numRows - noBlockCol == puzzle.columnClues(colIndex)) {
        for (rowIndex <- 0 until numRows) {
          val block = puzzle.grid(rowIndex)(colIndex)
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
        if(block.state.contains(1) && !block.isFullyKnown) {
          if (rowIndex == 0 && columnIndex == 0) {
            puzzle.grid(rowIndex)(columnIndex+1).state = Some(1)
            puzzle.grid(rowIndex+1)(columnIndex).state = Some(1)
          }
          else if (rowIndex == 0 && columnIndex == puzzle.size._2-1) {
            puzzle.grid(rowIndex)(columnIndex-1).state = Some(1)
            puzzle.grid(rowIndex+1)(columnIndex).state = Some(1)
          }
          else if (rowIndex == puzzle.size._1-1 && columnIndex == 0) {
            puzzle.grid(rowIndex-1)(columnIndex).state = Some(1)
            puzzle.grid(rowIndex)(columnIndex+1).state = Some(1)
          }
          else if (rowIndex == puzzle.size._1-1 && columnIndex == puzzle.size._2-1) {
            puzzle.grid(rowIndex-1)(columnIndex).state = Some(1)
            puzzle.grid(rowIndex)(columnIndex-1).state = Some(1)
          }else {


            val neighbours: Array[(Int, Int, Int)] = Array(
              (rowIndex, columnIndex - 1, 0), //left
              (rowIndex - 1, columnIndex, 1), //Up
              (rowIndex, columnIndex + 1, 2), //Right
              (rowIndex + 1, columnIndex, 3), //Down
            )

            // count number of no track neighbour blocks
            val count: Int = neighbours.count(neigh => {
              if (inBounds(neigh._1, neigh._2, puzzle.size._1, puzzle.size._2)) {
                puzzle.grid(neigh._1)(neigh._2).state.contains(0)
              } else
                false
            })
            //var directedNeighbours = new Array[(Int, Int)](4)
            //count number of fully known neighbour blocks
            val dirNeighCount: Int = neighbours.count { neigh =>
              if (inBounds(neigh._1, neigh._2, puzzle.size._1, puzzle.size._2)) {
                if (puzzle.grid(neigh._1)(neigh._2).isFullyKnown) {
                  //The left neighbor
                  if (neigh._3 == 0) {
                    puzzle.grid(neigh._1)(neigh._2).paths(Direction.Right).contains(0)
                  }
                  //The top neighbor
                  else if (neigh._3 == 1) {
                    puzzle.grid(neigh._1)(neigh._2).paths(Direction.Down).contains(0)
                  }
                  //The right neighbor
                  else if (neigh._3 == 2) {
                    puzzle.grid(neigh._1)(neigh._2).paths(Direction.Left).contains(0)
                  }
                  //The bottom neighbor
                  else if (neigh._3 == 3) {
                    puzzle.grid(neigh._1)(neigh._2).paths(Direction.Up).contains(0)
                  }
                  else {
                    false
                  }
                } else {
                  false
                }
              } else {
                false
              }
            }



            //if block is on boundary of matrix
            if (isOnBoundary(block, rowIndex, columnIndex, puzzle)) {
              //if block state is 1 check if neighbour Some(0) count is == 1 or neighbour Some(1) has different direction
              if (count == 1 || dirNeighCount==1) {
                for (neighbour <- neighbours) {
                  if (inBounds(neighbour._1, neighbour._2, puzzle.size._1, puzzle.size._2)) {
                    if (!puzzle.grid(neighbour._1)(neighbour._2).state.contains(0)) {
                      puzzle.grid(neighbour._1)(neighbour._2).state = Some(1)
                    }
                  }
                }
              }
            }
            // else the block will not be on boundary of matrix
            else if(count == 2 || dirNeighCount == 2){
              for (neighbour <- neighbours){
                if(!puzzle.grid(neighbour._1)(neighbour._2).state.contains(0)){
                  puzzle.grid(neighbour._1)(neighbour._2).state = Some(1)
                }
              }
            }
            else if (count == 1 && dirNeighCount == 1) {
              for (neighbour <- neighbours) {
                if (!puzzle.grid(neighbour._1)(neighbour._2).state.contains(0)) {
                  puzzle.grid(neighbour._1)(neighbour._2).state = Some(1)
                }
              }
            }
          }
        }
      }
    }
    puzzle
  }



  def connect(puzzle: Puzzle): Puzzle = {




    for((row, rowIndex) <- puzzle.grid.zipWithIndex){
      for((block, columnIndex) <- row.zipWithIndex) {
        if (!puzzle.grid(rowIndex)(columnIndex).isFullyKnown && puzzle.grid(rowIndex)(columnIndex).state.contains(1)) {
            val neighbours: Array[(Int, Int, Int)] = Array(
              (rowIndex, columnIndex - 1, 0), //left
              (rowIndex - 1, columnIndex, 1), //up
              (rowIndex, columnIndex + 1, 2), //right
              (rowIndex + 1, columnIndex, 3) //down
            )

            //Check for two neighbors exiting directly into the block
            val neighborsExitingIntoBlock = neighbours.filter {
              case (nRow, nCol, direction) if inBounds(nRow, nCol, puzzle.size._1, puzzle.size._2) =>
                val neighborBlock = puzzle.grid(nRow)(nCol)
                neighborBlock.state.contains(1) && neighborBlock.paths(direction match {
                  case 0 => Direction.Right
                  case 1 => Direction.Down
                  case 2 => Direction.Left
                  case 3 => Direction.Up
                }).contains(1)
              case _ => false
            }

            //If there are two neighbors with paths exiting into the block, connect to them
            if (neighborsExitingIntoBlock.length == 2) {
              var updatedBlock = block
              for ((nRow, nCol, direction) <- neighborsExitingIntoBlock) {
                direction match {
                  case 0 => updatedBlock = updatedBlock.updatePath(Direction.Left, 1)
                  case 1 => updatedBlock = updatedBlock.updatePath(Direction.Up, 1)
                  case 2 => updatedBlock = updatedBlock.updatePath(Direction.Right, 1)
                  case 3 => updatedBlock = updatedBlock.updatePath(Direction.Down, 1)
                }
                //println(s"Forced connection for ${rowIndex},${columnIndex} in direction $direction")
              }
              puzzle.grid(rowIndex)(columnIndex) = updatedBlock
            }

            var pathableNeighbour: Array[Int] = Array(0, 0, 0, 0)

            val pathable: Int = neighbours.count(neigh => {
              if (inBounds(neigh._1, neigh._2, puzzle.size._1, puzzle.size._2)) {
                if (puzzle.grid(neigh._1)(neigh._2).state.contains(0)) {
                  false
                } else if (puzzle.grid(neigh._1)(neigh._2).state.contains(1)) {
                  //check is path is known or not
                  if (puzzle.grid(neigh._1)(neigh._2).isFullyKnown) {
                    if (neigh._3 == 0 && puzzle.grid(neigh._1)(neigh._2).paths(Direction.Right).contains(1)) {
                      pathableNeighbour(0) = 1
                      true
                    } else if (neigh._3 == 1 && puzzle.grid(neigh._1)(neigh._2).paths(Direction.Down).contains(1)) {
                      pathableNeighbour(1) = 1
                      true
                    } else if (neigh._3 == 2 && puzzle.grid(neigh._1)(neigh._2).paths(Direction.Left).contains(1)) {
                      pathableNeighbour(2) = 1
                      true
                    } else if (neigh._3 == 3 && puzzle.grid(neigh._1)(neigh._2).paths(Direction.Up).contains(1)) {
                      pathableNeighbour(3) = 1
                      true
                    } else
                      false
                  } else
                    pathableNeighbour(neigh._3) = 1
                    true
                } else
                  false
              } else false
            })

            //println("(" + rowIndex + ")" + "(" + columnIndex + ")" + " - " + pathableNeighbour.mkString("Array(", ", ", ")"))





          if (pathable == 2) {
            var updatedBlock = block
            for ((nRow, nCol, direction) <- neighbours) {
              if (inBounds(nRow, nCol, puzzle.size._1, puzzle.size._2)) {
                val neighborBlock = puzzle.grid(nRow)(nCol)
                if (neighborBlock.isFullyKnown) {
                  if (direction == 0 && neighborBlock.paths(Direction.Right).contains(1)) {
                    updatedBlock = updatedBlock.updatePath(Direction.Left, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to left 1")
                  }
                  else if (direction == 1 && neighborBlock.paths(Direction.Down).contains(1)) {
                    updatedBlock = updatedBlock.updatePath(Direction.Up, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to up 1")
                  }
                  else if (direction == 2 && neighborBlock.paths(Direction.Left).contains(1)) {
                    updatedBlock = updatedBlock.updatePath(Direction.Right, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to right 1")
                  }
                  else if (direction == 3 && neighborBlock.paths(Direction.Up).contains(1)) {
                    updatedBlock = updatedBlock.updatePath(Direction.Down, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to down 1")
                  }
                } else if (neighborBlock.state.contains(1)) {
                  // Update based on neighbor state alone if not fully known
                  if (direction == 0) {
                    updatedBlock = updatedBlock.updatePath(Direction.Left, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to left 1")
                  }
                  if (direction == 1) {
                    updatedBlock = updatedBlock.updatePath(Direction.Up, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to up 1")
                  }
                  if (direction == 2) {
                    updatedBlock = updatedBlock.updatePath(Direction.Right, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to right 1")
                  }
                  if (direction == 3) {
                    updatedBlock = updatedBlock.updatePath(Direction.Down, 1)
                    //println(s"updated ${rowIndex},${columnIndex} to down 1")
                  }
                }
              }
            }
            puzzle.grid(rowIndex)(columnIndex) = updatedBlock
          }
          /*
          for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
            for ((block, colIndex) <- row.zipWithIndex) {
              val coordinates = s"($rowIndex, $colIndex)"

              if (block.isFullyKnown) {
                println(s"Block $coordinates: is fully known")
              }


            }
          }

           */

        }
      }
    }
    puzzle
  }

  def finishConnect(puzzle: Puzzle): Puzzle = {

    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex) {
        val count: Int = block.paths.values.count(_.contains(1))

        if (block.state.contains(1) && count == 0) {
          val neighbours: Array[(Int, Int, Int)] = Array(
            (rowIndex, colIndex - 1, 0), //left
            (rowIndex - 1, colIndex, 1), //up
            (rowIndex, colIndex + 1, 2), //right
            (rowIndex + 1, colIndex, 3)  //down
          )

          val viableNeighbours: Array[(Int, Int, Int)] = neighbours.filter { case (row, column, _) =>
            inBounds(row, column, puzzle.size._1, puzzle.size._2) &&
              puzzle.grid(row)(column).state.contains(1) &&
              puzzle.grid(row)(column).paths.values.forall {
                case Some(0) => true
                case None => true
                case _ => false
              }
          }

          var updatedBlock = block

          if (viableNeighbours.length == 2) {
            val (neighbour1Row, neighbour1Col, _) = viableNeighbours(0)
            val (neighbour2Row, neighbour2Col, _) = viableNeighbours(1)

            val isNeighbour1Loop = loopChecker(puzzle, rowIndex, colIndex, neighbour1Row, neighbour1Col)
            val isNeighbour2Loop = loopChecker(puzzle, rowIndex, colIndex, neighbour2Row, neighbour2Col)

            if (isNeighbour1Loop) {
              if (neighbour2Row == rowIndex && neighbour2Col == colIndex - 1) {
                updatedBlock = updatedBlock.updatePath(Direction.Left, 1)
              } else if (neighbour2Row == rowIndex - 1 && neighbour2Col == colIndex) {
                updatedBlock = updatedBlock.updatePath(Direction.Up, 1)
              } else if (neighbour2Row == rowIndex && neighbour2Col == colIndex + 1) {
                updatedBlock = updatedBlock.updatePath(Direction.Right, 1)
              } else if (neighbour2Row == rowIndex + 1 && neighbour2Col == colIndex) {
                updatedBlock = updatedBlock.updatePath(Direction.Down, 1)
              }
            } else if (isNeighbour2Loop) {
              if (neighbour1Row == rowIndex && neighbour1Col == colIndex - 1) {
                updatedBlock = updatedBlock.updatePath(Direction.Left, 1)
              } else if (neighbour1Row == rowIndex - 1 && neighbour1Col == colIndex) {
                updatedBlock = updatedBlock.updatePath(Direction.Up, 1)
              } else if (neighbour1Row == rowIndex && neighbour1Col == colIndex + 1) {
                updatedBlock = updatedBlock.updatePath(Direction.Right, 1)
              } else if (neighbour1Row == rowIndex + 1 && neighbour1Col == colIndex) {
                updatedBlock = updatedBlock.updatePath(Direction.Down, 1)
              }
            }
          }

          // Check for surrounding blocks to update paths if they connect to the current block
          if (inBounds(rowIndex, colIndex - 1, puzzle.size._1, puzzle.size._2) &&
            puzzle.grid(rowIndex)(colIndex - 1).paths(Direction.Right).contains(1)) {
            updatedBlock = updatedBlock.updatePath(Direction.Left, 1)
          }

          if (inBounds(rowIndex - 1, colIndex, puzzle.size._1, puzzle.size._2) &&
            puzzle.grid(rowIndex - 1)(colIndex).paths(Direction.Down).contains(1)) {
            updatedBlock = updatedBlock.updatePath(Direction.Up, 1)
          }

          if (inBounds(rowIndex, colIndex + 1, puzzle.size._1, puzzle.size._2) &&
            puzzle.grid(rowIndex)(colIndex + 1).paths(Direction.Left).contains(1)) {
            updatedBlock = updatedBlock.updatePath(Direction.Right, 1)
          }

          if (inBounds(rowIndex + 1, colIndex, puzzle.size._1, puzzle.size._2) &&
            puzzle.grid(rowIndex + 1)(colIndex).paths(Direction.Up).contains(1)) {
            updatedBlock = updatedBlock.updatePath(Direction.Down, 1)
          }

          puzzle.grid(rowIndex)(colIndex) = updatedBlock
        }
      }
    }

    puzzle
  }



  def loopChecker(puzzle: Puzzle, rowIndex: Int, colIndex: Int, targetRow: Int, targetCol: Int): Boolean = {
    var lastBlock: (Int, Int) = (rowIndex, colIndex)
    val origRowIdx: Int = rowIndex
    val origColIdx: Int = colIndex
    var startRowIdx: Int = rowIndex
    var startColIdx: Int = colIndex

    val neighbours: Array[(Int, Int, Int)] = Array(
      (rowIndex, colIndex - 1, 0), //left
      (rowIndex - 1, colIndex, 1), //up
      (rowIndex, colIndex + 1, 2), //right
      (rowIndex + 1, colIndex, 3)  //down
    )


    if (inBounds(neighbours(0)._1, neighbours(0)._2, puzzle.size._1, puzzle.size._2) &&
      puzzle.grid(neighbours(0)._1)(neighbours(0)._2).paths(Direction.Right).contains(1)) {
      startRowIdx = neighbours(0)._1
      startColIdx = neighbours(0)._2
    }
    else if (inBounds(neighbours(1)._1, neighbours(1)._2, puzzle.size._1, puzzle.size._2) &&
      puzzle.grid(neighbours(1)._1)(neighbours(1)._2).paths(Direction.Down).contains(1)) {
      startRowIdx = neighbours(1)._1
      startColIdx = neighbours(1)._2
    }
    else if (inBounds(neighbours(2)._1, neighbours(2)._2, puzzle.size._1, puzzle.size._2) &&
      puzzle.grid(neighbours(2)._1)(neighbours(2)._2).paths(Direction.Left).contains(1)) {
      startRowIdx = neighbours(2)._1
      startColIdx = neighbours(2)._2
    }
    else if (inBounds(neighbours(3)._1, neighbours(3)._2, puzzle.size._1, puzzle.size._2) &&
      puzzle.grid(neighbours(3)._1)(neighbours(3)._2).paths(Direction.Up).contains(1)) {
      startRowIdx = neighbours(3)._1
      startColIdx = neighbours(3)._2
    } else {
      //println("No initial move direction found, exiting.")
      return false
    }

    var iterationCount = 0


    while (startRowIdx != origRowIdx || startColIdx != origColIdx) {
      //println(s"Iteration: $iterationCount, Start: ($startRowIdx, $startColIdx), Last: $lastBlock")


      if (startRowIdx == targetRow && startColIdx == targetCol) {

        return true
      }

      if (iterationCount > 30) {

        return false
      }
      iterationCount += 1

      if (inBounds(startRowIdx, startColIdx, puzzle.size._1, puzzle.size._2)) {
        if (puzzle.grid(startRowIdx)(startColIdx).paths(Direction.Left).contains(1) && lastBlock != (startRowIdx, startColIdx - 1)) {
          lastBlock = (startRowIdx, startColIdx)
          startColIdx -= 1
        }
        else if (puzzle.grid(startRowIdx)(startColIdx).paths(Direction.Up).contains(1) && lastBlock != (startRowIdx + 1, startColIdx)) {
          lastBlock = (startRowIdx, startColIdx)
          startRowIdx -= 1
        }
        else if (puzzle.grid(startRowIdx)(startColIdx).paths(Direction.Right).contains(1) && lastBlock != (startRowIdx, startColIdx + 1)) {
          lastBlock = (startRowIdx, startColIdx)
          startColIdx += 1
        }
        else if (puzzle.grid(startRowIdx)(startColIdx).paths(Direction.Down).contains(1) && lastBlock != (startRowIdx - 1, startColIdx)) {
          lastBlock = (startRowIdx, startColIdx)
          startRowIdx += 1
        } else {

          return false
        }
      } else {

        return false
      }
    }

    false
  }





  def makeFullyKnown(puzzle: Puzzle): Puzzle = {
    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, columnIndex) <- row.zipWithIndex) {
        if (!puzzle.grid(rowIndex)(columnIndex).isFullyKnown) {
          val pathcount = block.paths.values.count(_.contains(1))
          if(pathcount == 2) {
            var updatedBlock = block
            val updatedPaths = block.paths.map {
              case (direction, value) if !value.contains(1) => {
                println(s"updated ${rowIndex} , ${columnIndex}" + direction + " " + value + "to 0")
                direction -> Some(0)
              }
              case other => other
            }
            updatedBlock = updatedBlock.copy(paths = updatedPaths)
            puzzle.grid(rowIndex)(columnIndex) = updatedBlock
          }
        }
      }
    }
    puzzle
  }

  /*
  for((row, rowIndex) <- puzzle.grid.zipWithIndex){
      for((block, columnIndex) <- row.zipWithIndex) {
   */


def extendParts(puzzle: Puzzle): Puzzle = {
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


}
