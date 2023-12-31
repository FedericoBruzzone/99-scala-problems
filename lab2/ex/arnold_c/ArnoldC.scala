// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./ArnoldC.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ArnoldCEvaluator print-10.arnoldc

import scala.util.parsing.combinator._
import scala.io._

class ArnoldCParser extends JavaTokenParsers {
  var symbol_table = Map[String, Int]()

  def program = "IT'S SHOWTIME" ~> rep(stmt) <~ "YOU HAVE BEEN TERMINATED"

  def stmt = print | init | assgn | if_then_else

  def print = "TALK TO THE HAND" ~> (unquotedString | value)
  def init = "HEY CHRISTMAS TREE" ~> id ~ ("YOU SET US UP" ~> value)
  def assgn = "GET TO THE CHOPPER" ~> id ~ ("HERE IS MY INVITATION" ~> value) ~ rep(expr) <~ "ENOUGH TALK"
  def if_then_else: Parser[Any] = "BECAUSE I'M GOING TO SAY PLEASE" ~> id ~ ("[" ~> rep(stmt)) ~ ("]" ~> "BULLSHIT" ~> "[" ~> rep(stmt)) ~ ("]" ~> "YOU HAVE NO RESPECT FOR LOGIC")

  def expr = arith | logic

  def arith = ( ("GET UP" ~> value) /* + */
              | ("GET DOWN" ~> value) /* - */
              | ("YOU'RE FIRED" ~> value) /* * */
              | ("HE HAD TO SPLIT" ~> value)) /* / */
  def logic = ( ("YOU ARE NOT YOU YOU ARE ME" ~> value) /* == */
              | ("LET OFF SOME STEAM BENNET" ~> value) /* > */
              | ("CONSIDER THAT A DIVORCE" ~> value) /* V */
              | ("KNOCK KNOCK" ~> value)) /* ^ */


  def unquotedString = stringLiteral ^^ { s => s.substring(1, s.length - 1) }
  def value = id | intNumber
  def id = """[a-zA-Z][a-zA-Z0-9_]*""".r // ^^ { x => symbol_table(x) }
  def intNumber = """-?[0-9]+""".r ^^ { _.toInt }
}

object ArnoldCEvaluator {
  def main(args: Array[String]) {
    val p = new ArnoldCParser
    args.foreach { arg =>
      val source = Source.fromFile(arg)
      val input = source.mkString
      p.parseAll(p.program, input) match {
        case p.Success(result, _) => result
        case p.Failure(msg, _) => println("FAILURE: " + msg)
        case p.Error(msg, _) => println("ERROR: " + msg)
      }
      source.close()
    }
  }
}

