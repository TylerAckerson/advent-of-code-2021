import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/day3/input.txt")
val lines = source.getLines.toList
source.close

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
