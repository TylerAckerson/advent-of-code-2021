import scala.annotation.tailrec
import scala.io.Source

val source =
  Source.fromFile(System.getProperty("user.dir") + "/day7/input.txt")
val lines = source.getLines.toList
source.close

val heights: List[Int] = (lines map { line =>
  {
    line.split(",").map(str => Integer.parseInt(str))
  }
}).flatten.sorted

// set the minGas to something very high
// from 0 to maxHeight:
//  calculate the sum for all crabs to get to that point: abs(a - b)
// if gasUsed < minGas, set minGas to gasUsed
def calculateMinGasNeeded(heights: List[Int]): Int = {
  val maxHeight = heights.max
  var minGas = heights.size * maxHeight
  var optimalHeight = heights.head

  for (height <- 0 to maxHeight) {
    val gasNeeded = heights
      .map(h => {
        Math.abs(h - height)
      })
      .sum

    println(s"gasNeeded for height $height: $gasNeeded")

    if (gasNeeded < minGas) {
      minGas = gasNeeded
      optimalHeight = height
    }
  }

  minGas
}

calculateMinGasNeeded(heights) // 354129
