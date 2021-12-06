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
def calculateOxygenGeneratorRating(
    binaryStrings: List[String],
    idx: Int = 0
): Option[Int] = {
  if (binaryStrings.length <= 1) {
    binaryStrings.foreach(println(_))
    return binaryStrings.headOption.map(Integer.parseInt(_, 2))
  }

  val mapped: List[(Char, String)] = binaryStrings.map { str =>
    (str(idx), str)
  }

  val (zeroCount, oneCount) = mapped.partition { case (h, _) => h == '0' }
  val most: Char = if (oneCount.size >= zeroCount.size) '1' else '0'
  val keepers: List[String] = mapped.filter {
    case (head, _) => {
      head == most
    }
  } map (a => a._2)

  calculateOxygenGeneratorRating(keepers, idx + 1)
}

def calculateCO2ScrubberRating(
    binaryStrings: List[String],
    idx: Int = 0
): Option[Int] = {
  if (binaryStrings.length <= 1) {
    binaryStrings.foreach(println(_))
    return binaryStrings.headOption.map(Integer.parseInt(_, 2))
  }

  val mapped: List[(Char, String)] = binaryStrings.map { str =>
    (str(idx), str)
  }

  val (zeroCount, oneCount) = mapped.partition { case (h, _) => h == '0' }
  val least: Char = if (zeroCount.size <= oneCount.size) '0' else '1'
  val keepers: List[String] = mapped.filter {
    case (head, _) => {
      head == least
    }
  } map (a => a._2)

  calculateCO2ScrubberRating(keepers, idx + 1)
}

val oxygenGeneratorRating = calculateOxygenGeneratorRating(lines)
val c02ScrubberRating = calculateCO2ScrubberRating(lines)

oxygenGeneratorRating.getOrElse(0) * c02ScrubberRating.getOrElse(0)

// TODO: lots of common code for these functions -> could be abstracted
