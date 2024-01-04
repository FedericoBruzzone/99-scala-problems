// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./Csv.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar CsvEvaulator books.csv
import scala.util.parsing.combinator._
import scala.io._

class CsvParser extends JavaTokenParsers {
  override def skipWhitespace = false
  // override protected val whiteSpace = """[ \t\f]+""".r

  def program = records
  def records = repsep(record, eol) ^^ { case rs => rs.dropRight(1) }
  def record = repsep(field, ",")
  def field = """[^,\n\r]*""".r
  def eol = "\n" | "\r\n"
}

class CsvPrinter(val records: List[List[String]]) {
  val colunms = records.transpose
  val title = records.head
  val rows = records.tail

  def maxPerCell(): List[Int] = { colunms.map { colunm => colunm.map { cell => cell.length }.max } }
  def padCell(cell: String, max: Int): String = { cell + (" " * (max - cell.length)) }
  def printDashedLine(maxs: List[Int]): String = { return "-" * (maxs.sum + (maxs.length * 3) + 1) }
  def printRow(r: List[String], max: List[Int]): String = { return "| " + r.zip(max).map { case (cell, max) => padCell(cell, max) }.mkString(" | ") + " |" }

  def pprint(): Unit = {
    val maxs = maxPerCell()
    println(printDashedLine(maxs))
    println(printRow(title, maxs))
    println(printDashedLine(maxs))
    rows.foreach(x => println(printRow(x, maxs)))
    println(printDashedLine(maxs))
  }
}

object CsvEvaulator {
  def main(args: Array[String]): Unit = {
    val p = new CsvParser
    println(p.stringLiteral)
    args.foreach { arg =>
      val source = Source.fromFile(arg)
      val lines = try source.mkString finally source.close()
      p.parseAll(p.program, lines) match {
        case p.Success(r, _) => //println(r)
          val printer = new CsvPrinter(r)
          printer.pprint
        case x => println(x)
      }
      source.close()
    }
  }
}

