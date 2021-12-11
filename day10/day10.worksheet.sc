import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.mutable.HashSet

val source =
  Source.fromFile(System.getProperty("user.dir") + "/day10/input.txt")
val lines = source.getLines.toList
source.close

val CharMap: Map[Char, Char] = Map(
  '(' -> ')',
  '[' -> ']',
  '{' -> '}',
  '<' -> '>'
)
val ReverseCharMap: Map[Char, Char] = (Map() ++ CharMap.map(_.swap))

val CharPointsCorrupted: Map[Char, Int] = Map(
  ')' -> 3,
  ']' -> 57,
  '}' -> 1197,
  '>' -> 25137
)

val CharPointsIncomplete: Map[Char, Long] = Map(
  ')' -> 1,
  ']' -> 2,
  '}' -> 3,
  '>' -> 4
)

val OpenSet: Set[Char] = Set.from(CharMap.keys)
val CloseSet: Set[Char] = Set.from(CharMap.values)

abstract class SyntaxResult(
    mismatches: Stack[Char],
    firstMismatch: Option[Char]
)
case class Corrupted(mismatches: Stack[Char], firstMismatch: Some[Char])
    extends SyntaxResult(mismatches, firstMismatch)
case class Incomplete(mismatches: Stack[Char], firstMismatch: Option[Char])
    extends SyntaxResult(mismatches, None)

def getSyntaxResultForString(str: String): SyntaxResult = {
  var mismatch: Option[Char] = None
  val stack = new Stack[Char]

  str.foreach { char =>
    {
      val matchingChar = CharMap.getOrElse(char, ReverseCharMap(char))
      if (OpenSet.contains(char)) {
        stack.push(char)
      } else if (CloseSet.contains(char)) {
        if (stack.isEmpty) {
          mismatch = Some(char)
        } else if (stack.top == matchingChar) {
          stack.pop
        } else if (mismatch.isEmpty) {
          mismatch = Some(char)
        }
      }
    }
  }

  if (mismatch.isDefined) {
    Corrupted(stack, Some(mismatch.get))
  } else {
    Incomplete(stack, mismatch)
  }
}

def scoreIncomplete(incomplete: Incomplete): Long = {
  incomplete.mismatches.toList
    .map { char =>
      {
        CharPointsIncomplete(CharMap(char))
      }
    } reduce { case (acc: Long, v: Long) => (acc * 5) + v }
}

val (corrupted: List[Corrupted], incompletes: List[Incomplete]) =
  lines.partitionMap { str =>
    getSyntaxResultForString(str) match {
      case c: Corrupted  => Left(c)
      case i: Incomplete => Right(i)
    }
  }

/*
 * PART ONE
 */
val corruptedSum: Int = corrupted
  .flatMap(c => {
    c.firstMismatch.map(CharPointsCorrupted)
  })
  .sum // 318099

/*
 * PART TWO
 */
val incompleteScores =
  incompletes.map(scoreIncomplete).sorted
val medianIdx = incompleteScores.length / 2
incompleteScores(medianIdx) //2389738699
