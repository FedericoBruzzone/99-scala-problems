// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./mylang.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar MyLangEvaluator ./test.mylang

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.io._

class MyLangParser(var vars: Map[String, Int],
                   var funcs: Map[String, (List[Int]) => Any]) extends JavaTokenParsers {
  funcs += ("print" -> ((l: List[Int]) => println(l.head)))
  var m = Map[String, (Int, Int) => Int](
    "+" -> ((_: Int) + (_: Int)),
    "-" -> ((_: Int) - (_: Int)),
    "*" -> ((_: Int) * (_: Int)),
    "/" -> ((_: Int) / (_: Int)),
  )

  def program = stmts ^^ { case x => x() }
  def stmts = rep(stmt) ^^ { case l => () => l.foreach(_()) }

  def stmt: Parser[() => Any] = def_fun | call_fun | assign | expr

  def def_fun = "def" ~> ident ~ ("(" ~> repsep(ident, ",") <~ ")") ~ ("{" ~> stmts <~ "}") ^^ { case id ~ pars ~ body => () =>
    funcs += (id -> ((l: List[Int]) => {
        var old_vars = vars
        vars = Map() ++ pars.zip(l).toMap // vars = Map[String, Int](pars.zip(l): _*)
        body()
        vars = old_vars
      })
    )
  }

  def call_fun = ident ~ ("(" ~> repsep(term, ",") <~ ")") ^^ { case id ~ l => () => funcs(id)(l.map(_())) }

  def assign = ident ~ ("=" ~> expr) ^^ { case id ~ e => () => vars += (id -> e()) }

  def expr: Parser[() => Int] = fact ~ ("+" | "-") ~ expr ^^ { case a ~ op ~ b => () => m(op)(a(), b()) } | fact
  def fact: Parser[() => Int] = term ~ ("*" | "/") ~ fact ^^ { case a ~ op ~ b => () => m(op)(a(), b()) } | term
  def term = (ident ^^ { case id => () => vars(id) }
                                 | wholeNumber ^^ { case num => () => num.toInt }
                                 | "(" ~> expr <~ ")")
}

object MyLangEvaluator {
  def main(args: Array[String]): Unit = {
    val p = new MyLangParser(Map(), Map())
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

