// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./pn.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./PNEvaluator ./pn.txt


import scala.util.parsing.combinator._
import scala.io._
import scala.collection.mutable._

class PNParser(var s: Stack[Int]) extends JavaTokenParsers {
  val map = Map("+" -> ((x: Int, y: Int) => x + y),
                "-" -> ((x: Int, y: Int) => x - y),
                "x" -> ((x: Int, y: Int) => x * y),
                "/" -> ((x: Int, y: Int) => x / y))

  def program = exprs ^^ { case x => x.map { x => x(); s.pop } }

  // def exprs = rep(expr)
  // def expr: Parser[Int] = op ~ expr ~ expr ^^ { case op ~ e1 ~ e2 => map(op)(e1, e2) } | num
  // def op = ("+" | "-" | "x" | "/")
  // def num = """[1-9]""".r ^^ { case x => x.toInt }

  def exprs = rep(expr)
  def expr: Parser[() => Any] = op ~ expr ~ expr ^^ { case op ~ e1 ~ e2 => () => { e1(); e2(); op() } } | num
  def op: Parser[() => Any] = ("+" | "-" | "x" | "/") ^^ { case x => () => { var v1 = s.pop; var v2 = s.pop; s.push(map(x)(v2, v1)) } }
  def num: Parser[() => Any] = """[1-9]""".r ^^ { case x => () => s.push(x.toInt) }
}

object PNEvaluator {
  def main(args: Array[String]): Unit = {
    args.foreach { arg =>
      val p = new PNParser(new Stack[Int])
      val source = Source.fromFile(arg)
      val input = source.mkString
      p.parseAll(p.program, input) match {
        case p.Success(result, _) => println(result)
        case x => println(x)
      }
      source.close()
    }
  }
}

