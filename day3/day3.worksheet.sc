import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/day3/input.txt")
val lines = source.getLines.toList
source.close

/*
 * PART ONE
 */
var bitCounts = Array.fill(12)(Array(0, 0))
lines.foreach { line =>
  {
    val bytes: Array[Int] = line.toCharArray.map(_.asDigit)
    bytes.zipWithIndex foreach {
      case (bit: Int, idx: Int) => {
        bitCounts(idx)(bit) += 1
      }
    }
  }
}

var gammaBytes = ""
var epsilonBytes = ""
bitCounts foreach { counts =>
  {
    var zeroCount = counts(0)
    var oneCount = counts(1)

    if (zeroCount >= oneCount) {
      gammaBytes += "0"
      epsilonBytes += "1"
    } else {
      gammaBytes += "1"
      epsilonBytes += "0"
    }
  }

}

val gamma = Integer.parseInt(gammaBytes, 2)
val epsilon = Integer.parseInt(epsilonBytes, 2)

gamma * epsilon

/*
 * PART TWO
 */

// common method for calculations
private def calculateRating(
    binaryStrings: List[String],
    comparison: (Int, Int) => Char,
    idx: Int = 0
): Option[Int] = {
  if (binaryStrings.length <= 1) {
    binaryStrings.foreach(println(_))
    return binaryStrings.headOption.map(Integer.parseInt(_, 2))
  }

  val mapped: List[(Char, String)] = binaryStrings.map { str =>
    (str(idx), str)
  }

  val (zeros, ones) = mapped.partition { case (h, _) => h == '0' }
  val target: Char = comparison(zeros.size, ones.size)
  val keepers: List[String] = mapped.collect {
    case (char: Char, str: String) if char == target => str
  }

  calculateRating(keepers, comparison, idx + 1)
}
def calculateOxygenGeneratorRating(binaryStrings: List[String]): Option[Int] = {
  val oxygenComparison = (zeroCount: Int, oneCount: Int) =>
    if (zeroCount <= oneCount) '1' else '0'

  calculateRating(binaryStrings, oxygenComparison)
}

def calculateCO2ScrubberRating(
    binaryStrings: List[String],
    idx: Int = 0
): Option[Int] = {
  val scrubberComparison = (zeroCount: Int, oneCount: Int) =>
    if (zeroCount <= oneCount) '0' else '1'

  calculateRating(binaryStrings, scrubberComparison)
}

val oxygenGeneratorRating = calculateOxygenGeneratorRating(lines)
val c02ScrubberRating = calculateCO2ScrubberRating(lines)

oxygenGeneratorRating.getOrElse(0) * c02ScrubberRating.getOrElse(0)
