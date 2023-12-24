// scala -cp .:./scala-parser-combinators_2.13-2.3.0.jar ./arithmetic_operations_as_in_the_primary_school.scala ./1_arithmetic_operations_as_in_the_primary_school.input

import scala.util.parsing.combinator._

class TestParser extends JavaTokenParsers {
  def start: Parser[Any] = "" ^^ { _ => "start" }
}

object TestMain {
  def main(args: Array[String]) = {
    val p = new TestParser
    args.foreach { filename =>
      val src = scala.io.Source.fromFile(filename)
      val lines = src.mkString
      p.parseAll(p.start, lines) match {
        case p.Success(result, _) => print(result.toString)
        case x => print(x.toString)
      }
      src.close()
    }
  }
}
