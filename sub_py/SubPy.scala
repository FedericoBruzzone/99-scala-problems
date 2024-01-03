// scalac -cp .:../lab2/scala-parser-combinators_2.13-2.3.0.jar SubPy.scala && scala -cp .:../lab2/scala-parser-combinators_2.13-2.3.0.jar SubPyEvaluator test.py

import scala.util.parsing.combinator._

class SubPyParser extends JavaTokenParsers {
  // skip comments
  override protected val whiteSpace = """(\s|#.*)+""".r

  def program: Parser[Any] = statements

  def statements: Parser[Any] = rep(statement)

  def statement: Parser[Any] = assignment | print | ifelse

  def assignment: Parser[Any] = ident ~ "=" ~ expression

  def print: Parser[Any] = "print" ~ "(" ~ expression ~ ")"

  def ifelse: Parser[Any] = "if" ~ expression ~ ":" ~ statements ~ "else" ~ ":" ~ statements

  def expression: Parser[Any] = term ~ rep(("+" | "-") ~ term)

  def term: Parser[Any] = factor ~ rep(("*" | "/" | "%") ~ factor)

  def factor: Parser[Any] = wholeNumber | ident | "(" ~ expression ~ ")"
}

object SubPyEvaluator {
  def main(args: Array[String]): Unit = {
    val p = new SubPyParser
    args.foreach { arg =>
      val source = scala.io.Source.fromFile(arg)
      val lines = try source.mkString finally source.close()
      p.parseAll(p.program, lines) match {
        case p.Success(result, _) => println(result)
        case p.NoSuccess(msg, _) => println(msg)
      }
    }
  }
}

