// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./Wtf.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar WtFEvaluator ops.wtf

import scala.util.parsing.combinator._

class WtfParser extends RegexParsers {
  def program: Parser[Any] = rep(stmt)

  def stmts = rep(stmt)
  // def funs = rep(fun)
  //
  // def fun = "def" ~ fun_name ~ args ~ "=" ~ body_fun
  def fun_name = """[A-Z]""".r
  // def args = """[0-9]""".r
  // def body_fun = ""

  def stmt =  print | expr

  def expr = fun_call | add_sub

  def add_sub = "0" ~ rep(("+" | "-")) | "0"  // | tern_if

  def fun_call = rep(add_sub) ~ fun_name

  // def tern_if = expr ~ "?" ~ if_body ~ ":" ~ if_body
  // def if_body = "[" ~ stmts ~ "]"

  def print = expr ~ "!"
}


object WtfEvaluator {
  def main(args: Array[String]): Unit = {
    val p = new WtfParser
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
