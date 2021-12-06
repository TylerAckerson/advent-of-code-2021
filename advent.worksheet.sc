import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/input.txt")
val lines = source.getLines.toList.map(_.toDouble)
source.close

var increaseCount = 0;
var lastChunkTotal = lines.take(3).sum

for (idx <- 1 to lines.length - 3) {
  val chunkTotal = List(lines(idx), lines(idx + 1), lines(idx + 2)).sum

  if (chunkTotal > lastChunkTotal)
    increaseCount += 1

  lastChunkTotal = chunkTotal
}

increaseCount
