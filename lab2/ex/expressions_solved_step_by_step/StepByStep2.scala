// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar StepByStep2.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar StepByStepEvaluator "((2 + 7) + ((3 + 9) + 4))" "((1 * 7) + (7 * ((3 + 9) + 5)))" "((5*(7-2))+(((15/3)+2)-((1422*2)-(3500/4))))"

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.io._

sealed trait Tree {
  val m = Map[String, (Int, Int) => Int](
    "+" -> ((_: Int) + (_: Int)), // "+" -> ((x: Int, y: Int) => (x + y)),
    "-" -> ((_: Int) - (_: Int)),
    "*" -> ((_: Int) * (_: Int)),
    "/" -> ((_: Int) / (_: Int)),
  )
}
case class Leaf(n: Int) extends Tree { override def toString(): String = { return if (n == 0) "" else n.toString } }
case class Internal(l: Tree, r: Tree, op: String) extends Tree { override def toString(): String = { return "(" + l.toString + op + r.toString + ")" } }

class StepByStepParser extends JavaTokenParsers {
  def program = expr
  def expr: Parser[Tree] = fact ~ ("+" | "-") ~ expr ^^ { case x ~ op ~ y => Internal(x, y, op) } | fact
  def fact: Parser[Tree] = term ~ ("*" | "/") ~ fact ^^ { case x ~ op ~ y => Internal(x, y, op) } | term
  def term: Parser[Tree] = ("(" ~> expr <~ ")"
                           | wholeNumber ^^ { case x => Leaf(x.toInt) }
                           | "-" ~> fact ^^ { case x => Internal(Leaf(0), x, "-") })
}

class StepByStepPrinter {
  def reduce(t: Tree): Tree = {
    t match {
      case Internal(Leaf(l), Leaf(r), op: String) => Leaf(t.m(op)(l, r))
      case Leaf(l) => Leaf(l)
      case Internal(a, b, op) => Internal(reduce(a), reduce(b), op)
    }
  }

  def pprint(t: Tree): Unit = {
    println(t)
    if (t.isInstanceOf[Leaf]) {return}
    pprint(reduce(t))
  }
}

object StepByStepEvaluator {
  def main(args: Array[String]) {
    val p = new StepByStepParser
    args.foreach { arg =>
      p.parseAll(p.program, arg) match {
        case p.Success(tree, _) => { //println(tree); println() }
          var printer = new StepByStepPrinter
          printer.pprint(tree)
          println()
        }
        case e: p.NoSuccess => println(e)
      }
    }
  }
}

