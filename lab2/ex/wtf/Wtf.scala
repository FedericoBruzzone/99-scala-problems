// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./Wtf.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar WtfEvaluator ops.wtf
import scala.util.parsing.combinator._

class WtfParser extends RegexParsers {
  def program: Parser[Any] = funs ~ stmts ^^ {
      case _ ~ y => y.foreach { x => x() }
    }

  def funs = rep(fun)
  def fun = ("def" ~> fun_name) ~ args ~ ("=" ~> body_fun)
  def fun_name = """[A-Z]""".r
  def args = """[0-9]""".r
  def body_fun = stmts
  def arg_call = """[$][0-9]+""".r

  def stmts: Parser[List[() => Any]] = rep(stmt)
  def stmt =  (tern_if | print | expr) ^^ { case x => () => x() }
  def expr = fun_call | add_sub
  def add_sub = (arg_call | "0") ~ rep(("+" | "-")) ^^ {
      case x ~ q => x match {
        case "0" => () => inc_dec(0, q)()
        case _ => () => 0
      }
    }
  def fun_call = rep(add_sub) ~ fun_name ^^ {
      case x ~ y => () => 0
    }
  def tern_if = expr ~ ("?" ~> if_body <~ ":") ~ if_body ^^ {
      case x ~ y ~ z => () => if (x != 0) y else z
    }
  def if_body = "[" ~> stmts <~ "]"
  def print = expr <~ "!" ^^ { case e => () => println(e) }

  def inc_dec(start: Int, q: List[String]): () => Int = {
    () => q.foldLeft(start) {
      case (acc, "+") => acc + 1
      case (acc, "-") => acc - 1
    }
  }
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
