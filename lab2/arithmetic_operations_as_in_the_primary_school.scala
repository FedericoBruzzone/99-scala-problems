// scala -cp .:./scala-parser-combinators_2.13-2.3.0.jar ./arithmetic_operations_as_in_the_primary_school.scala ./1_arithmetic_operations_as_in_the_primary_school.input ./2_arithmetic_operations_as_in_the_primary_school.input ./3_arithmetic_operations_as_in_the_primary_school.input

import scala.util.parsing.combinator._


class TestParser extends JavaTokenParsers {
  def start = {
    operation ^^ {case (n, r) => eval(n) == r}
  }

  def operation: Parser[(List[BigInt], BigInt)] = wholeNumber ~ rep(number) ~ result ^^ {case f~n~r => (BigInt(f) :: n, r)}
  def number = ("+" | "-") ~ wholeNumber ^^ {case s~n => BigInt(s + n)}
  def result = "=" ~> dashline ~> opt("-") ~ wholeNumber ^^ {case s~n => BigInt(s.getOrElse("") + n)}
  def dashline: Parser[Any] = """[-]+""".r

  def eval(n: List[BigInt]): BigInt = {
    n match {
      case Nil => 0
      case x :: Nil => x
      case x :: y :: xs => eval((x + y) :: xs)
    }
  }
}

object TestMain {
  def main(args: Array[String]) = {
    val p = new TestParser
    args.foreach { filename =>
      val src = scala.io.Source.fromFile(filename)
      val lines = src.mkString
      p.parseAll(p.start, lines) match {
        case p.Success(result, _) => println(result.toString)
        case x => print(x.toString)
      }
      src.close()
    }
  }
}
