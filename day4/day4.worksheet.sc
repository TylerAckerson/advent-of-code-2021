import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/day4/input.txt")
val lines = source.getLines.toList
source.close

case class Board(lines: List[List[Int]])
object Board {
  def fromLines(lines: List[String]): Board = {
    val splitLines = lines.map(line => {
      line.split(" ").toList.collect {
        case s if !s.trim.isEmpty => Integer.parseInt(s, 10)
      }
    })
    Board(splitLines)
  }

  def isSolved(board: Board, calledNumbers: List[Int]): Boolean = {

    def solvedHorizontally(board: Board, calledNumbers: List[Int]): Boolean = {
      board.lines.exists(line =>
        line.forall(value => calledNumbers.contains(value))
      )
    }

    def solvedVertically(board: Board, calledNumbers: List[Int]): Boolean = {
      for (idx <- 0 to 4) {
        if (board.lines.forall(l => calledNumbers.contains(l(idx)))) {
          return true
        }
      }
      false
    }

    solvedHorizontally(board, calledNumbers) || solvedVertically(
      board,
      calledNumbers
    )
  }

  def getScore(board: Board, called: List[Int], lastCalled: Int): Int = {
    var score = 0;
    board.lines.flatten.foreach { num =>
      {
        if (!called.contains(num)) {
          score += num
        }
      }
    }

    score * lastCalled
  }
}

def callUntilWinner(
    boards: List[Board],
    called: List[Int],
    left: List[Int]
): (Board, List[Int], Int) = {
  var solved: Option[Board] = boards.find(Board.isSolved(_, called))

  if (solved.isEmpty) {
    callUntilWinner(boards, called :+ left.head, left.drop(1))
  } else {
    (solved.get, called, called.last)
  }

}

val callNumbers: List[Int] =
  lines.head.split(",").toList.map(Integer.parseInt(_, 10))
val boardLines = lines.tail.collect { case str if str.trim.size != 0 => str }
val boards = boardLines.grouped(5).toList.map { list => Board.fromLines(list) }

var called = callNumbers.take(5)
var left = callNumbers.drop(5)
val (winningBoard, winningNumbers, lastCalled) =
  callUntilWinner(boards, called, left)

Board.getScore(winningBoard, winningNumbers, lastCalled)

/*
 * PART TWO
 */
