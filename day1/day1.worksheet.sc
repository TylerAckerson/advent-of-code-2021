import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/day1/input.txt")
val lines = source.getLines.toList.map(_.toDouble)
source.close

var increaseCount = 0;
var start :: rest = lines

rest foreach { value =>
  {
    if (value > start)
      increaseCount += 1

    start = value
  }
}

increaseCount
