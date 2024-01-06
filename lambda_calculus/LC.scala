// scalac -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar ./LC.scala && scala -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar LCEvaluator first.lc

import scala.util.parsing.combinator._

trait LC
case class Var(name: String) extends LC
case class Abs(v: LC, body: LC) extends LC
case class App(fun: LC, arg: LC) extends LC

class LCParser extends JavaTokenParsers {
  override protected val whiteSpace = """(\s|#.*)+""".r

  def program = exprs
  def exprs = rep(expr)

  def expr: Parser[LC] = app
  def app: Parser[LC] = abs ~ app ^^ { case e1 ~ e2 => App(e1, e2) } | abs
  def abs: Parser[LC] = "λ" ~ varr ~ "." ~ expr ^^ { case "λ" ~ v ~ "." ~ e => Abs(v, e)} | varr
  def varr: Parser[LC] = """[a-z]""".r ^^ { case v => Var(v) } | "(" ~> expr <~ ")"
} 

object LCEvaluator {
  def main(args: Array[String]) = {
    val p = new LCParser
    args.foreach { arg =>
      val source = scala.io.Source.fromFile(arg)
      val input = source.mkString
      p.parseAll(p.program, input) match {
        case p.Success(result, _) => println(result)
        case p.Failure(msg, _) => println("FAILURE: " + msg)
        case p.Error(msg, _) => println("ERROR: " + msg)
      }
      source.close()
    }
  }
}
