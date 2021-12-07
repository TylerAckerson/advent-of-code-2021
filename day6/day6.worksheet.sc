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

fish.size // 300

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
