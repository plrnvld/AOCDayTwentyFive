import scala.io.Source
import scala.collection.mutable.HashMap

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Example2.txt").getLines.toList
        println(s"${lines.length} with first line length ${lines(0).length}")

        var board = new Board()
        board.addLines(lines)

        println(s"Board size is ${board.size()}")
        println(s"${board.get(0, 0)} with index (0, 0)")
        println(s"${board.get(0, 1)} with index (0, 1)")
        println(s"${board.get(1, 1)} with index (1, 1)")
        println(s"${board.get(2, 1)} with index (2, 1)")
        println(s"${board.get(3, 1)} with index (3, 1)")
        println(s"${board.get(9, 8)} with index (9, 8)")
    }
    
    class Board {
        var boardMap: HashMap[(Int, Int), Boolean] = new HashMap()
        var width = 0
        var height = 0

        def addLines(lines: List[String]) = {
            width = lines(0).length
            height = lines.length
            var x = 0
            var y = 0
            for (line <- lines) {
                for (c <- line) {
                    if (c == '>') {
                        addCucumberRight(x, y)
                    } else if (c == 'v') {
                        addCucumberDown(x, y)
                    }

                    x += 1
                }

                x = 0
                y += 1
            }
        }

        def addCucumberRight(x: Int, y: Int) = addCucumber(true, x, y)

        def addCucumberDown(x: Int, y: Int) = addCucumber(false, x, y)

        def addCucumber(movesRight: Boolean, x: Int, y: Int) 
            = boardMap((x, y)) = movesRight

        def get(key: (Int, Int)): Option[Boolean] = boardMap.get(key)

        def nextMove(key: (Int, Int)): Option[(Int, Int)] = {
            val (x, y) = key
            val boardItem = get(key)
            val nextKey = boardItem match {
                case Some(movesRight) if movesRight  => ((x + 1) % width, y)
                case Some(movesRight) if !movesRight => (x, (y + 1) % height)
                case _ => throw new RuntimeException("n must be a multiple of 10")
              }

            // Incorrect
            Some(nextKey)
        }

        def size(): Int = boardMap.size
    }
}
