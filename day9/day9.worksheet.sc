import scala.io.Source
import scala.collection.mutable.ListBuffer

val source =
  Source.fromFile(System.getProperty("user.dir") + "/day9/input.txt")
val lines = source.getLines.toList
source.close

val grid: List[List[Int]] = lines.map { line =>
  line.split("").toList.map(Integer.parseInt(_))
}

// 100 x 100 grid
// iterate through each point, check above, below, left, and right (if they exist)
// if the current point is lower than all neighbors, it is a low point (store it)
// if any point adjacent is lower, move on to the next point
var lowPoints = new ListBuffer[Int]()
for (x <- 0 until grid.head.length) {
  for (y <- 0 until grid.length) {
    val currentHeight = grid(x)(y)

    val aboveCoord = if (y - 1 >= 0) Some(x, y - 1) else None
    val belowCoord = if (y + 1 < 100) Some(x, y + 1) else None
    val leftCoord = if (x - 1 >= 0) Some(x - 1, y) else None
    val rightCoord = if (x + 1 < 100) Some(x + 1, y) else None

    val isLowest =
      List(aboveCoord, belowCoord, leftCoord, rightCoord).flatten.forall {
        coord =>
          {
            val height = grid(coord._1)(coord._2)
            currentHeight < height
          }
      }

    if (isLowest) {
      lowPoints += currentHeight
    }
  }
}

lowPoints.map(_ + 1).sum
