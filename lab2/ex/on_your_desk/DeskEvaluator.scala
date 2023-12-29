// scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./DeskEvaluator.scala test.desk

import scala.util.parsing.combinator._

class DeskParser extends JavaTokenParsers {
  var symbol_table = Map[String, Int]()

  def program = statement

  def statement = ("print" ~> expression <~ "where") ~ repsep(assignment, ",") ^^ {
    case e ~ _ => println(e()); symbol_table
  }

  def expression: Parser[() => Int] = (factor ~ ("+" | "-") ~ expression) ^^ {
    _ match {
      case f ~ "+" ~ e => () => f() + e()
      case f ~ "-" ~ e => () => f() - e()
      case _ => () => 0
    }
  } | factor
  def factor: Parser[() => Int] = (term ~ ("*" | "/") ~ factor) ^^ {
    _ match {
      case t ~ "*" ~ f => () => t() * f()
      case t ~ "/" ~ f => () => t() / f()
      case _ => () => 1
    }
  } | term
  def term: Parser[() => Int] = (number | ident ^^ { case x => () => symbol_table(x) })
  def number = "-?[0-9]+".r ^^ { case x => () => x.toInt }

  def assignment = ident ~ ("=" ~> wholeNumber) ^^ { case x ~ n => symbol_table += (x -> n.toInt) }
}

object Main {
  def main(args: Array[String]): Unit = {
    val p = new DeskParser
    args.foreach { arg =>
      val source = scala.io.Source.fromFile(arg)
      val input = source.mkString
      p.parseAll(p.program, input) match {
        case p.Success(result, _) => println(result)
        case p.NoSuccess(msg, next) => println(s"Parse error: $msg")
      }
      source.close()
    }
  }
}
