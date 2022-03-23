import scala.io.Source
import scala.collection.mutable.HashMap

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Example2.txt").getLines.toList
        println(s"${lines.length} with first line length ${lines(0).length}")

        var board = new Board()
        board.addLines(lines)

        println(s"Board size is ${board.size()}")
        println(s"${board.get(0, 0)} with index ${board.toIndex(0, 0)}")
        println(s"${board.get(0, 1)} with index ${board.toIndex(0, 1)}")
        println(s"${board.get(1, 1)} with index ${board.toIndex(1, 1)}")
        println(s"${board.get(2, 1)} with index ${board.toIndex(2, 1)}")
        println(s"${board.get(3, 1)} with index ${board.toIndex(3, 1)}")
        println(s"${board.get(9, 8)} with index ${board.toIndex(9, 8)}")
    }
    
    class Board {
        var boardMap: HashMap[Int, Boolean] = new HashMap()
        var width = 0

        def addLines(lines: List[String]) = {
            width = lines(0).length
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

        def addCucumberRight(x: Int, y: Int) 
            = addCucumber(true, x, y)

        def addCucumberDown(x: Int, y: Int) 
            = addCucumber(false, x, y)

        def addCucumber(movesRight: Boolean, x: Int, y: Int) 
            = boardMap(toIndex(x, y)) = movesRight

        def toIndex(x: Int, y: Int): Int = y * width + x

        def get(x: Int, y: Int): Option[Boolean] = boardMap.get(toIndex(x, y))

        def fromIndex(index: Int): (Int, Int) = (index % width, index / width)

        def size(): Int = boardMap.size
    }
}
