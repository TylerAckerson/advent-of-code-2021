import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/day2/input.txt")
val lines = source.getLines.toList
source.close

var depth = 0
var horizontalPosition = 0

lines.foreach { line =>
  {
    val segments = line.split(" ")
    val direction = segments.head
    val amount = segments.tail.head.toInt

    direction match {
      case "up"      => depth -= amount
      case "down"    => depth += amount
      case "forward" => horizontalPosition += amount
    }
  }
}

depth * horizontalPosition
