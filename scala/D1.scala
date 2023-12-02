object P1:
  val parseLine: String => Int = (line) =>
    val onlyDigits = line.toCharArray.filter(c => c.isDigit)
    val firstLast = onlyDigits(0).toString + onlyDigits.last.toString
    firstLast.toInt

  val sum: List[String] => Int = (lines) =>
    lines.map(parseLine).sum

object P2:
  val tokens = List(
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "0",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "zero"
  )

  val toDigit: String => String = token =>
    token match
      case "one" | "1" => "1"
      case "two" | "2" => "2"
      case "three" | "3" => "3"
      case "four" | "4" => "4"
      case "five" | "5" => "5"
      case "six" | "6" => "6"
      case "seven" | "7" => "7"
      case "eight" | "8" => "8"
      case "nine" | "9" => "9"
      case "zero" | "0" => "0"
      case _ => throw IllegalArgumentException(s"Invalid string ${token}")

  val parseLine: String => Int = (line) =>
    val (first, firstI) = tokens
      .map(t => (t, line.indexOf(t)))
      .filter((t, i) => i >= 0)
      .minBy((t, i) => i)
    val (last, lastI) = tokens
      .map(t => (t, line.lastIndexOf(t)))
      .filter((t, i) => i >= 0)
      .maxBy((t, i) => i)
    val firstLast = toDigit(first) + toDigit(last)
    firstLast.toInt

  val sum: List[String] => Int = (lines) =>
    lines.map(parseLine).sum

@main
def main(): Unit =
  val lines = scala.io.Source
    .fromFile("../input/d1.txt")
    .getLines
    .toList
  println(s"Part 1: ${P1.sum(lines)}")
  println(s"Part 2 ${P2.sum(lines)}")
