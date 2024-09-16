// PuzzleChecker.scala

object PuzzleChecker {

  // method to check if any row is fully filled according to the row clues
  def isFullRow(puzzle: Puzzle, rowIndex: Int): Boolean = {
    val clue = puzzle.rowClues(rowIndex)
    val width = puzzle.size._1
    width == clue // check if the width matches the clue
  }

  // method to check if any column is fully filled according to the column clues
  def isFullColumn(puzzle: Puzzle, colIndex: Int): Boolean = {
    val clue = puzzle.columnClues(colIndex)
    val height = puzzle.size._2
    height == clue // check if the number of filled blocks matches the clue
  }

  // utility function to get the opposite direction
  def opposite(direction: Direction.Value): Direction.Value = direction match {
    case Direction.Left => Direction.Right
    case Direction.Right => Direction.Left
    case Direction.Up => Direction.Down
    case Direction.Down => Direction.Up
  }

  // function that gets the neighbors of a block at (rowIndex, colIndex)
  def getNeighbors(puzzle: Puzzle, rowIndex: Int, colIndex: Int): Map[Direction.Value, Block] = {
    Direction.values.toSeq.flatMap { dir =>
      val (newRowIndex, newColIndex) = dir match {
        case Direction.Left => (rowIndex, colIndex - 1)
        case Direction.Up => (rowIndex - 1, colIndex)
        case Direction.Right => (rowIndex, colIndex + 1)
        case Direction.Down => (rowIndex + 1, colIndex)
      }
      if (inBounds(newRowIndex, newColIndex, puzzle.size._2, puzzle.size._1)) {
        Some(dir -> puzzle.grid(newRowIndex)(newColIndex))
      } else {
        None
      }
    }.toMap
  }

  // function that updates possible tracks for each block based on neighbor block
  def updatePossibleTracks(puzzle: Puzzle): Puzzle = {
    val newGrid = puzzle.grid.map(_.clone())

    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex if block.state.isEmpty) {
        val neighbors = getNeighbors(puzzle, rowIndex, colIndex)
        val updatedPossibleTracks = block.possibleTracks.filter { trackType =>
          trackType.connects.forall { dir =>
            neighbors.get(dir) match {
              case Some(neighbor) =>
                neighbor.state match {
                  case Some(1) =>

                    neighbor.possibleTracks.exists(_.connects.contains(opposite(dir)))
                  case Some(0) =>
                    false
                  case None =>
                    true
                }
              case None =>
                false
            }
          }
        }
        newGrid(rowIndex)(colIndex) = block.copy(possibleTracks = updatedPossibleTracks)
      }
    }

    puzzle.copy(grid = newGrid)
  }

  def inBounds(row: Int, col: Int, maxRows: Int, maxCols: Int): Boolean = {
    row >= 0 && row < maxRows && col >= 0 && col < maxCols
  }

  def extendParts(puzzle: Puzzle): Puzzle = {
    // create a copy of the grid rather than modify the original grid
    val newGrid = puzzle.grid.map(_.clone())

    for ((row, rowIndex) <- puzzle.grid.zipWithIndex) {
      for ((block, colIndex) <- row.zipWithIndex) {
        val pathCount = block.paths.values.count {
          case Some(1) => true
          case _ => false
        }
        if (block.state.contains(1) && pathCount == 2) {
          val directions = block.paths.collect {
            case (dir, Some(1)) => dir
          }
          directions.foreach { dir =>
            val (newRowIndex, newColIndex) = dir match {
              case Direction.Left => (rowIndex, colIndex - 1)
              case Direction.Up => (rowIndex - 1, colIndex)
              case Direction.Right => (rowIndex, colIndex + 1)
              case Direction.Down => (rowIndex + 1, colIndex)
            }
            if (inBounds(newRowIndex, newColIndex, puzzle.size._2, puzzle.size._1)) {
              // update the block in the new grid
              val neighborBlock = newGrid(newRowIndex)(newColIndex)
              newGrid(newRowIndex)(newColIndex) = neighborBlock.copy(state = Some(1))
            }
          }
        }
      }
    }
    puzzle.copy(grid = newGrid)
  }
  
  def isPathConnected(puzzle: Puzzle): Boolean = {
    val numRows = puzzle.grid.length
    val numCols = puzzle.grid(0).length
    val visited = Array.fill(numRows, numCols)(false)
    
    // to perform a depth-first search, we identify a starting point
    val start = findStartingBlock(puzzle)
    start match {
      case Some((row, col)) =>
        dfs(puzzle, row, col, visited)
        
        // have all track blocks been visited?
        for (rowIndex <- 0 until numRows) {
          for (colIndex <- 0 until numCols) {
            val block = puzzle.grid(rowIndex)(colIndex)
            if (block.state.contains(1) && !visited(rowIndex)(colIndex)) {
              // found an unvisited block
              return false
            }
          }
        }
        true // all track blocks are connected
      case None =>
        true // no track blocks in the puzzle
    }
  }
  
  def findStartingBlock(puzzle: Puzzle): Option[(Int, Int)] = {
    for (rowIndex <- puzzle.grid.indices) {
      for (colIndex <- puzzle.grid(0).indices) {
        if (puzzle.grid(rowIndex)(colIndex).state.contains(1)) {
          return Some((rowIndex, colIndex))
        }
      }
    }
    None
  }
  
  // depth-first seach function
  def dfs(puzzle: Puzzle, row: Int, col: Int, visited: Array[Array[Boolean]]): Unit = {
    if (!inBounds(row, col, puzzle.size._2, puzzle.size._1)) return
    if (visited(row)(col)) return
    val block = puzzle.grid(row)(col)
    if (!block.state.contains(1)) return
    
    visited(row)(col) = true
    
    // visit connected neighbors
    for (dir <- block.paths.keys) {
      if (block.paths(dir).contains(1)) {
        val (newRow, newCol) = dir match {
          case Direction.Left     => (row, col - 1)
          case Direction.Right    => (row, col + 1)
          case Direction.Up       => (row - 1, col)
          case Direction.Down     => (row + 1, col)
        }
        if (inBounds(newRow, newCol, puzzle.size._2, puzzle.size._1)) {
          val neighbor = puzzle.grid(newRow)(newCol)
          if (neighbor.state.contains(1) && neighbor.paths(opposite(dir)).contains(1)) {
            dfs(puzzle, newRow, newCol, visited)
          }
        }
      }
    }
  }
  
  def labelConnectedComponents(puzzle: Puzzle): Array[Array[Int]] = {
    val numRows = puzzle.grid.length
    val numCols = puzzle.grid(0).length
    val labels = Array.fill(numRows, numCols)(-1)
    var label = 0
    
    for (row <- 0 until numRows) {
      for (col <- 0 until numCols) {
        if (puzzle.grid(row)(col).state.contains(1) && labels(row)(col) == -1) {
          dfsLabel(puzzle, row, col, labels, label)
          label += 1
        }
      }
    }
    labels
  }
  
  def dfsLabel(puzzle: Puzzle, row: Int, col: Int, labels: Array[Array[Int]], label: Int): Unit = {
    if (!inBounds(row, col, puzzle.size._2, puzzle.size._1)) return
    if (labels(row)(col) != -1) return
    val block = puzzle.grid(row)(col)
    if (!block.state.contains(1)) return
    
    labels(row)(col) = label
    
    for (dir <- block.paths.keys) {
      if (block.paths(dir).contains(1)) {
        val (newRow, newCol) = dir match {
          case Direction.Left   => (row, col - 1)
          case Direction.Right  => (row, col + 1)
          case Direction.Up     => (row - 1, col)
          case Direction.Down   => (row + 1, col)
        }
        if (inBounds(newRow, newCol, puzzle.size._2, puzzle.size._1)) {
          val neighbor = puzzle.grid(newRow)(newCol)
          if (neighbor.state.contains(1) && neighbor.paths(opposite(dir)).contains(1)) {
            dfsLabel(puzzle, newRow, newCol, labels, label)
          }
        }
      }
    }
  }
  
  def findConnectorBlocks(puzzle: Puzzle, labels: Array[Array[Int]]): List[(Int, Int, Set[Int])] = {
    val numRows = puzzle.grid.length
    val numCols = puzzle.grid(0).length
    var connectors = List.empty[(Int, Int, Set[Int])]
    
    for (row <- 0 until numRows) {
      for (col <- 0 until numCols) {
        val block = puzzle.grid(row)(col)
        if (block.state.isEmpty || block.state.contains(1)) {
          val neighborLabels = getNeighbors(puzzle, row, col).flatMap {
            case (dir, neighbor) =>
              val (nRow, nCol) = dir match {
                case Direction.Left   => (row, col - 1)
                case Direction.Right  => (row, col + 1)
                case Direction.Up     => (row - 1, col)
                case Direction.Down   => (row + 1, col)
              }
              if (inBounds(nRow, nCol, puzzle.size._2, puzzle.size._1)) {
                val label = labels(nRow)(nCol)
                if (label != -1) Some(label) else {
                  None
                }
            } else {
                None
              }
          }.toSet
          
          if (neighborLabels.size > 1) {
            // block is adjacent to multiple connected components
            connectors ::= (row, col, neighborLabels)
          }
        }
      }
    }
    connectors
  }
  
  def enforceConnectivity(puzzle: Puzzle): Puzzle = {
    val labels = labelConnectedComponents(puzzle)
    val connectors = findConnectorBlocks(puzzle, labels)
    val newGrid = puzzle.grid.map(_.clone())
    
    for ((row, col, neighborLabels) <- connectors) {
      val block = newGrid(row)(col)
      val neighbors = getNeighbors(puzzle, row, col)
      val updatedPossibleTracks = block.possibleTracks.filter { trackType =>
        // can the trackType connect two different components?
        val connectsToLabels = trackType.connects.flatMap { dir =>
          neighbors.get(dir) match {
            case Some(neighbor) =>
              val (nRow, nCol) = dir match {
                case Direction.Left   => (row, col - 1)
                case Direction.Right  => (row, col + 1)
                case Direction.Up     => (row - 1, col)
                case Direction.Down   => (row + 1, col)
              }
              if (inBounds(nRow, nCol, puzzle.size._2, puzzle.size._1)) {
                val label = labels(nRow)(nCol)
                if (label != -1) Some(label) else None
              } else None
            case None => None
          }
        }
        connectsToLabels.size >= 2
      }
      
      if (updatedPossibleTracks.nonEmpty) {
        newGrid(row)(col) = block.copy(possibleTracks = updatedPossibleTracks)
      }
    }
    
    puzzle.copy(grid = newGrid)
  }
}