import scala.annotation.tailrec
import scala.io.Source

val source = Source.fromFile(System.getProperty("user.dir") + "/day6/input.txt")
val lines = source.getLines.toList
source.close

val fish: List[Int] = (lines map { line =>
  {
    line.split(",").map(str => Integer.parseInt(str))
  }
}).flatten.sorted

/*
 * PART ONE
 */
def ageOneDay(fish: List[Int]): List[Int] = {
  val sorted = fish.sorted
  val ageZeroCount = sorted.view.count(_ == 0)

  sorted.map { daysLeft =>
    {
      if (daysLeft == 0) 6 else daysLeft - 1
    }
  } ++ List.fill(ageZeroCount)(8)
}

@tailrec
private final def ageNDays(n: Int, fish: List[Int]): List[Int] = {
  if (n == 0)
    return fish

  ageNDays(n - 1, ageOneDay(fish))
}

val agedEightyDays = ageNDays(80, fish)
agedEightyDays.size // 362666

/*
 * PART TWO
 */
def ageOneDayCounts(fishCounts: List[Long]): List[Long] = {
  val zeros = fishCounts.head
  val newCounts = fishCounts.tail.toArray :+ 0.toLong

  newCounts(6) = newCounts(6) + zeros
  newCounts(8) = zeros

  newCounts.toList
}

@tailrec
private final def ageNDaysCounts(
    n: Int,
    fishCounts: List[Long]
): List[Long] = {
  if (n == 0)
    return fishCounts

  ageNDaysCounts(n - 1, ageOneDayCounts(fishCounts))
}

def fishToCounts(fish: List[Int]): List[Long] = {
  val arr = Array.fill(9)(0)
  fish foreach { f =>
    {
      arr(f) += 1
    }
  }

  arr.map(_.toLong).toList
}

val fishCounts = fishToCounts(fish) // List(0, 119, 45, 48, 40, 48, 0, 0, 0)
fishCounts.sum // 300

val countsEighty = ageNDaysCounts(80, fishCounts)
countsEighty.sum // 362666

val counts256 = ageNDaysCounts(256, fishCounts)
counts256.sum // Long = 1640526601595
