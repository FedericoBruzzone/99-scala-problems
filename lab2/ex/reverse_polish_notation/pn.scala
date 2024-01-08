// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./pn.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar PNEvaluator ./pn.txt


import scala.util.parsing.combinator._
import scala.io._
import scala.collection.mutable._

class PNParser(var s: Stack[Int]) extends JavaTokenParsers {
  override val whiteSpace = """[ \t\r\f]+""".r

  val map = Map("+" -> ((x: Int, y: Int) => x + y),
                "-" -> ((x: Int, y: Int) => x - y),
                "x" -> ((x: Int, y: Int) => x * y),
                "/" -> ((x: Int, y: Int) => x / y))

  def program = stmts

  def stmts = rep(stmt)

  // def stmt = """.*\n""".r ^^ { x => parseAll(expr, x.dropRight(1)); s.pop }
  def stmt = expr <~ "\n" ^^ { x => s.pop }

  def expr = rep(op | num)
  def op = "+" | "-" | "x" | "/" ^^ { x => s.push(map(x)(s.pop, s.pop)) }
  def num = """[0-9]""".r ^^ { x => s.push(x.toInt) }
}

object PNEvaluator {
  def main(args: Array[String]): Unit = {
    args.foreach { arg =>
      val p = new PNParser(new Stack[Int])
      val source = Source.fromFile(arg)
      val input = source.mkString
      println(input.split("\n").map(x => x.length).toList.max)
      p.parseAll(p.program, input) match {
        case p.Success(result, _) => println(result)
        case x => println(x)
      }
      source.close()
    }
  }
}

