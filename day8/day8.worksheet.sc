import scala.annotation.tailrec
import scala.io.Source

val source =
  Source.fromFile(System.getProperty("user.dir") + "/day8/input.txt")
val lines = source.getLines.toList
source.close

case class IO(strings: List[String])
case class IOLine(input: IO, output: IO)

lines.size

val IOLines: List[IOLine] = (lines map { line =>
  {
    val inputOutputArray = line.split(" \\| ")
    println(s"inputOutputArray: ${inputOutputArray(0)}")
    val input: List[String] = inputOutputArray(0).split(" ").toList
    val output: List[String] = inputOutputArray(1).split(" ").toList

    IOLine(IO(input), IO(output))
  }
})

// number -> segmentCount
val segmentCounts: Map[Int, Int] = Map(
  1 -> 2,
  4 -> 4,
  7 -> 3,
  8 -> 7
)

val counts = IOLines
  .map(line => {
    line.output.strings.count { string =>
      segmentCounts.values.toList.contains(string.length)
    }
  })
  .sum
