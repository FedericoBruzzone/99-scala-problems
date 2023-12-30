// scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar StepByStepEvaluator.scala "((2 + 7) + ((3 + 9) + 4))" "((1 * 7) + (7 * ((3 + 9) + 5)))" "((5*(7-2))+(((15/3)+2)-((1422*2)-(3500/4))))"

import scala.util.parsing.combinator._

sealed trait Tree {
  val ops = Map[String, (Int, Int) => Int](
    "+" -> ((_: Int) + (_: Int)),
    "-" -> ((_: Int) - (_: Int)),
    "*" -> ((_: Int) * (_: Int)),
    "/" -> ((_: Int) / (_: Int))
  )
}
final case class InternalNode(a: Tree, b: Tree, op: String) extends Tree {
  override def toString(): String = { "(" + a + op + b + ")" }
}
final case class Leaf(v: Int) extends Tree {
  override def toString(): String = { v.toString }
}

class StepByStepParser extends JavaTokenParsers {
  def program = expr

  def expr: Parser[Tree] = (fact ~ ("+" | "-") ~ expr) ^^ { case f ~ op ~ e => InternalNode(f, e, op) } | fact
  def fact: Parser[Tree]= (term ~ ("*" | "/") ~ fact) ^^ { case t ~ op ~ f => InternalNode(t, f, op) } | term
  def term: Parser[Tree] = (number | "(" ~> expr <~ ")")
  def number: Parser[Tree] = """-?[0-9]+""".r ^^ { case v => Leaf(v.toInt) }
}

object StepByStepPrinter {
  def print(t: Tree): Unit = {
    println(t)
    if (t.isInstanceOf[Leaf]) { return }
    print(reduction(t))
  }

  def reduction(t: Tree): Tree = {
    t match {
      case InternalNode(Leaf(a), Leaf(b), op: String) => Leaf(t.ops(op)(a, b).toInt)
      case InternalNode(a, b, op) => InternalNode(reduction(a), reduction(b), op)
      case Leaf(v) => Leaf(v)
    }
  }
}

object StepByStepEvaluator {
  def main(args: Array[String]) {
    val p = new StepByStepParser
    args.foreach { arg =>
      p.parseAll(p.program, arg) match {
        case p.Success(tree, _) => { // println(tree)
          StepByStepPrinter.print(tree)
          println()
        }
        case e: p.NoSuccess => println(e)
      }
    }
  }
}
