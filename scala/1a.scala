val parseLine : String => Int = (line) =>
  val onlyDigits = line.toCharArray.filter(c => c.isDigit)
  val firstLast = onlyDigits(0).toString + onlyDigits.last.toString
  firstLast.toInt

@main
def main(): Unit =
  val src = scala.io.Source.fromFile("../input/1.txt")
  val lines = src.getLines()
  println(lines.map(parseLine).sum)