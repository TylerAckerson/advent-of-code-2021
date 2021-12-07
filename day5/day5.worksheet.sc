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

var mapped = lineCoords
  .map({ lineCoord =>
    {
      if (lineCoord.start.x == lineCoord.end.x) {
        // iterate y axis
        var sorted = List(lineCoord.start.y, lineCoord.end.y).sorted
        Range.inclusive(sorted(0), sorted(1)).map { yCoord =>
          {
            Coord(lineCoord.start.x, yCoord)
          }
        }

      } else if (lineCoord.start.y == lineCoord.end.y) {
        // iterate x axis
        var sorted = List(lineCoord.start.x, lineCoord.end.x).sorted
        Range.inclusive(sorted(0), sorted(1)).map { xCoord =>
          {
            Coord(xCoord, lineCoord.start.y)
          }
        }
      } else {
        // diagonal - increment x and y at the same time
        var sorted = List(lineCoord.start, lineCoord.end).sortBy(_.x)
        var (start, end) = (sorted(0), sorted(1)) // so X is always increasing

        var xIdx = start.x
        // if y is decreasing, it needs to be explicit
        val step = if (end.y < start.y) -1 else 1
        Range.inclusive(start.y, end.y, step) map { yIdx =>
          {
            val coord = Coord(xIdx, yIdx)
            xIdx += 1 // side-effect inside a map :face_palm:
            coord
          }
        }
      }

    }
  })
  .flatten
  .groupBy { case (c: Coord) => c }
  .view
  .mapValues(_.size)

mapped.size //part one: 98,455, part two: 170,788
mapped count { case (_, count) => count > 1 } //part one: 7297 , part two: 21038
