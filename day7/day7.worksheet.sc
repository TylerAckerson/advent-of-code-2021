import scala.annotation.tailrec
import scala.io.Source

val source =
  Source.fromFile(System.getProperty("user.dir") + "/day7/input.txt")
val lines = source.getLines.toList
source.close

val locations: List[Int] = (lines map { line =>
  {
    line.split(",").map(str => Integer.parseInt(str))
  }
}).flatten.sorted

// set the minGas to something very high
// from 0 to maxLocation:
//  calculate the sum for all crabs to get to that point: abs(a - b)
// if gasUsed < minGas, set minGas to gasUsed
def calculateMinGasNeeded(distances: List[Int]): Int = {
  val maxLocation = locations.max
  var minGas = locations.size * sumAllInts(maxLocation)

  for (location <- 0 to maxLocation) {
    val gasNeeded = locations
      .map(y => { sumAllInts(Math.abs(y - location)) })
      .sum

    if (gasNeeded < minGas) {
      minGas = gasNeeded
    }
  }

  minGas
}

private def sumAllInts(n: Int): Int = (n * (n + 1)) / 2

calculateMinGasNeeded(locations) // 98905973

// Optimizations:
// start at the avg
//      try +1, try -1... try +2, try -2
//      if the gas keeps increasing, break out
// find a formula to calculate this without O(n^2)
