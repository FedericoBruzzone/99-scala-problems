// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./Desk2.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./DeskEvaluator test.desk

import scala.util.parsing.combinator._
import scala.io._

class DeskParser extends JavaTokenParsers {
  var m = Map[String, Int]()
  var mop = Map[String, (Int, Int) => Int]("+" -> ((_: Int) + (_: Int)), "-" -> ((_: Int) - (_: Int)))

  def program: Parser[Any] = "print" ~> expr <~ "where" <~ inits ^^ { e => (e(), m) }
  def expr: Parser[() => Int] = term ~ ("+" | "-") ~ expr ^^ { case t ~ op ~ e => () => mop(op.toString)(t(), e()) } | term
  def term: Parser[() => Int] = wholeNumber ^^ { x => () => x.toInt } | ident ^^ { case i => () => m(i) }
  def inits: Parser[Any] = repsep(init, ",")
  def init: Parser[Any] = ident ~ ("=" ~> wholeNumber) ^^ { case v ~ n => m += (v -> n.toInt) }
}

object DeskEvaluator {
  def main(args: Array[String]): Unit = {
    val p = new DeskParser
    args.foreach { arg =>
      val source = Source.fromFile(arg)
      val input = source.mkString
      p.parseAll(p.program, input) match {
        case p.Success(result, _) => println(result)
        case p.NoSuccess(msg, next) => println(s"Parse error: $msg")
      }
      source.close()
    }
  }
}
