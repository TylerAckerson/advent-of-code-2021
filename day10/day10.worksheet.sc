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

val CharPoints: Map[Char, Int] = Map(
  ')' -> 3,
  ']' -> 57,
  '}' -> 1197,
  '>' -> 25137
)

val OpenSet: Set[Char] = Set.from(CharMap.keys)
val CloseSet: Set[Char] = Set.from(CharMap.values)

def getFirstMismatch(str: String): Option[Char] = {
  var mismatch: Option[Char] = None

  val stack = new Stack[Char]
  stack.push(str.head)

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

  mismatch
}

val mismatches = lines.map(getFirstMismatch).flatten

mismatches.map(CharPoints).sum //318099
