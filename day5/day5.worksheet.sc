import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/day5/input.txt")
val lines = source.getLines.toList
source.close

case class Coord(x: Int, y: Int)
case class Line(start: Coord, end: Coord)

val lineCoords: List[Line] = lines.map { line =>
  {
    val lineCoords: Array[Coord] = line
      .split("->")
      .map { coords =>
        {
          val xAndY = coords.trim.split(",")
          val x = Integer.parseInt(xAndY(0), 10)
          val y = Integer.parseInt(xAndY(1), 10)

          Coord(x, y)
        }
      }
    Line(start = lineCoords(0), end = lineCoords(1))
  }
}

var mapped = collection.mutable.Map[Coord, Int]().withDefaultValue(0)
lineCoords
  .foreach({ lineCoord =>
    {
      if (lineCoord.start.x == lineCoord.end.x) {
        // iterate y axis
        var sorted = List(lineCoord.start.y, lineCoord.end.y).sorted
        for (yCoord <- sorted(0) to sorted(1)) {
          val coord = Coord(lineCoord.start.x, yCoord)
          mapped(coord) += 1
        }

      } else if (lineCoord.start.y == lineCoord.end.y) {
        // iterate x axis
        var sorted = List(lineCoord.start.x, lineCoord.end.x).sorted
        for (xCoord <- sorted(0) to sorted(1)) {
          val coord = Coord(xCoord, lineCoord.start.y)
          mapped(coord) += 1
        }
      } else {
        // diagonal - increment x and y at the same time
        var sorted = List(lineCoord.start, lineCoord.end).sortBy(_.x)
        var (start, end) = (sorted(0), sorted(1)) // so X is always increasing

        var xIdx = start.x
        // if y is decreasing, it needs to be explicit
        val step = if (end.y < start.y) -1 else 1
        for (yIdx <- start.y to end.y by step) {
          val coord = Coord(xIdx, yIdx)
          mapped(coord) += 1
          xIdx += 1
        }
      }

    }
  })

mapped.size //part one: 98,455, part two: 170,788
mapped count { case (_, count) => count > 1 } //part one: 7297 , part two: 21038
