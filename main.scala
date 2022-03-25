import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        
        var board = new Board()
        board.addLines(lines)

        while (board.step()) {
            // board.printBoard()
        }
        
        println("Final board")
        board.printBoard()
    }

    type Key = (Int, Int)
    type Move = (Boolean, Key, Key)
    
    class Board {
        var boardMap: HashMap[(Int, Int), Boolean] = new HashMap()
        var width = 0
        var height = 0
        var stepNum = 0

        def addLines(lines: List[String]) = {
            width = lines(0).length
            height = lines.length
            var x = 0
            var y = 0
            for (line <- lines) {
                for (c <- line) {
                    if (c == '>') {
                        boardMap((x, y)) = true
                    } else if (c == 'v') {
                        boardMap((x, y)) = false
                    }

                    x += 1
                }

                x = 0
                y += 1
            }
        }

        def step(): Boolean = {
            stepNum += 1

            println(s"Step $stepNum")
            
            var hasMoved = false

            val (keysRight, keysDown) = availableKeys()
            val movesRight = keysRight.map(nextMove).flatten
            if (!movesRight.isEmpty)
                hasMoved = true

            movesRight.foreach { applyMove }

            val movesDown = keysDown.map(nextMove).flatten
            if (!movesDown.isEmpty)
                hasMoved = true

            movesDown.foreach { applyMove }

            hasMoved
        }

        def applyMove(move: Move) = {
            val (moveRight, key, nextKey) = move

            boardMap.remove(key)
            boardMap(nextKey) = moveRight
        }

        def availableKeys(): (Seq[Key], Seq[Key]) = {

            var rightKeys = new ListBuffer[Key]()
            var downKeys = new ListBuffer[Key]()
            
            for ((k, v) <- boardMap) 
                if (v) rightKeys += k else downKeys += k

            (rightKeys.toList, downKeys.toList)
        }

        def get(key: (Int, Int)): Option[Boolean] = boardMap.get(key)

        def isFree(key: Key): Boolean = get(key).isEmpty

        def nextMove(key: Key): Option[Move] = {
            val (x, y) = key
            val boardItem = get(key)
            val (toRight, nextKey) = boardItem match {
                case Some(movesRight) if movesRight  => (true, ((x + 1) % width, y))
                case Some(movesRight) if !movesRight => (false, (x, (y + 1) % height))
                case _ => throw new RuntimeException(s"No cucumber found on $key")
              }

            Option.when(isFree(nextKey))((toRight, key, nextKey))
        }

        def size(): Int = boardMap.size

        def printBoard() = {
            for (y <- 0 until height) {
                for (x <- 0 until width) {
                    val item = get((x, y))
                    val itemChar = item match {
                        case Some(true) => '>'
                        case Some(false) => 'v'
                        case None => '.'
                    }

                    print(itemChar)                    
                }

                println()
            }
        }
    }
}
