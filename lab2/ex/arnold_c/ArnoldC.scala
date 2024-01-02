// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./ArnoldC.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ArnoldCEvaluator print-10.arnoldc

import scala.util.parsing.combinator._
import scala.io._
import scala.collection.mutable._

class ArnoldCParser extends JavaTokenParsers {
  var symbol_table = Map[String, Int]()
  var stack = Stack[Int]()

  def program = "IT'S SHOWTIME" ~> stmts <~ "YOU HAVE BEEN TERMINATED" ^^ { x => x(); symbol_table }

  def stmts: Parser[() => Any] = rep(stmt) ^^ { x => () => x.foreach { stmt => stmt() } }
  def stmt = print | init | if_then_else | assgn  | loop
  def print = "TALK TO THE HAND" ~> (id | value | unquotedString) ^^
    { case id => () => println(id()) }
  def init= "HEY CHRISTMAS TREE" ~> """[a-zA-Z][a-zA-Z0-9_]*""".r ~ ("YOU SET US UP" ~> value) ^^
    { case id ~ value => () => symbol_table += (id -> value()) }
  def if_then_else = "BECAUSE I'M GOING TO SAY PLEASE" ~> id ~ ("[" ~> stmts) ~ ("]" ~> "BULLSHIT" ~> "[" ~> stmts) <~ "]" <~ "YOU HAVE NO RESPECT FOR LOGIC" ^^
    { case id ~ if_stmts ~ else_stmts => () => if (id() != 0) if_stmts() else else_stmts() }
  def assgn = "GET TO THE CHOPPER" ~> """[a-zA-Z][a-zA-Z0-9_]*""".r ~ ("HERE IS MY INVITATION" ~> value) ~ exprs <~ "ENOUGH TALK" ^^
    { case id ~ value ~ exprs => () => { stack.push(value()); exprs(); symbol_table += (id -> stack.pop()) } }
  def loop = "STICK AROUND" ~> id ~ ("[" ~> stmts) <~ "]" <~ "CHILL" ^^
    { case id ~ stmts => () => while (id() != 0) stmts() }

  def exprs: Parser[() => Any] = rep(expr) ^^ { x => () => x.foreach { expr => expr() } }
  def expr = arith | logic
  def arith = ( "GET UP" ~> value /* + */ ^^ { x => () => { stack.push(stack.pop() + x()) } }
              | "GET DOWN" ~> value /* - */ ^^ { x => () => { stack.push(stack.pop() - x()) } }
              | "YOU'RE FIRED" ~> value /* * */ ^^ { x => () => { stack.push(stack.pop() * x()) } }
              | "HE HAD TO SPLIT" ~> value /* / */ ^^ { x => () => { stack.push(stack.pop() / x()) } })
  def logic = ( "YOU ARE NOT YOU YOU ARE ME" ~> value /* == */ ^^ { x => () => { stack.push(if (stack.pop() == x()) 1 else 0) } }
              | "LET OFF SOME STEAM BENNET" ~> value /* > */ ^^ { x => () => { stack.push(if (stack.pop() > x()) 1 else 0) } }
              | "CONSIDER THAT A DIVORCE" ~> value /* V */ ^^ { x => () => { stack.push(if (stack.pop() * x() == 0) 1 else 0) } }
              | "KNOCK KNOCK" ~> value /* ^ */ ^^ { x => () => { stack.push(if (stack.pop() * x() != 0) 1 else 0) } })

  def value: Parser[() => Int] = intNumber | id
  def id: Parser[() => Int] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { x => () => symbol_table(x) }
  def intNumber: Parser[() => Int] = wholeNumber ^^ { x => () => x.toInt }
  def unquotedString: Parser[() => String] = stringLiteral ^^ { x => () => x.substring(1, x.length - 1) }
}

object ArnoldCEvaluator {
  def main(args: Array[String]) {
    val p = new ArnoldCParser
    args.foreach { arg =>
      val source = Source.fromFile(arg)
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

