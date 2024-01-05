// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./ArnoldC2.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ArnoldCEvaluator print-10.arnoldc

import scala.util.parsing.combinator._
import scala.io._
import scala.collection.mutable._

class ArnoldCParser(var s: Stack[Int], 
                    var st: HashMap[String, Int]) extends JavaTokenParsers {

    def program = "IT'S" ~> "SHOWTIME" ~> stmts <~ "YOU" <~ "HAVE" <~ "BEEN" <~ "TERMINATED"
    
    // ================================================================================
    def stmts: Parser[Any] = rep(stmt)
    def stmt = print | init_var | assign_var | if_else | loop
    def print = "TALK" ~> "TO" ~> "THE" ~> "HAND" ~> (stringLiteral | val_of_var) ^^ 
        { case x => println(x) }
    def init_var = ("HEY" ~> "CHRISTMAS" ~> "TREE" ~> ident <~ "YOU" <~ "SET" <~ "US" <~ "UP") ~ value ^^ 
        { case k ~ v => st += (k -> v)}
    def assign_var = "GET" ~> "TO" ~> "THE" ~> "CHOPPER" ~> ident <~ "HERE" <~  "IS" <~ "MY" <~ "INVITATION" <~ push_value <~ exprs <~ "ENOUGH" <~ "TALK" ^^ 
        { case k => st += (k -> s.pop()) }
    def if_else = ("BECAUSE" ~>"I'M" ~>"GOING"~> "TO" ~>"SAY"~> "PLEASE" ~> ident) ~ body ~ ("BULLSHIT" ~> body <~ "YOU" <~ "HAVE"<~  "NO" <~ "RESPECT"<~  "FOR" <~ "LOGIC") ^^ 
        { case k ~ b1 ~ b2 => if (st(k) != 0) { parseAll(stmts, b1) } else { parseAll(stmts, b2) }}
    def loop = "STICK" ~> "AROUND" ~> ident ~ body <~ "CHILL" ^^ 
        { case k ~ b => while (st(k) != 0) { parseAll(stmts, b) } }
    // ================================================================================
    def exprs = rep(expr)
    def expr = arith_expr | logic_expr

    def arith_expr = add | sub | mul | div
    def add = "GET" ~> "UP" ~> value                    ^^ { case x => s.push(s.pop() + x)}
    def sub = "GET" ~> "DOWN" ~> value                  ^^ { case x => s.push(s.pop() - x)}
    def mul = "YOU'RE" ~> "FIRED" ~> value              ^^ { case x => s.push(s.pop() * x)}
    def div = "HE" ~> "HAD" ~> "TO" ~> "SPLIT" ~> value ^^ { case x => s.push((s.pop() / x).toInt)}

    def logic_expr: Parser[Any] = equal | greather | or | and 
    def equal = "YOU" ~> "ARE"~> "NOT" ~>"YOU" ~>"YOU"~> "ARE"~> "ME"~> value ^^ { case x => if (s.pop() == x) { s.push(1) } else { s.push(0) }}
    def greather = "LET"~> "OFF"~> "SOME" ~>"STEAM"~> "BENNET"~> value        ^^ { case x => if (s.pop() > x) { s.push(1) } else { s.push(0) }}
    def or = "CONSIDER" ~>"THAT"~> "A"~> "DIVORCE"~> value                    ^^ { case x => s.push(s.pop() * x)}
    def and = "KNOCK" ~> "KNOCK"~> value                                      ^^ { case x => s.push(s.pop() + x)}
    // ================================================================================

    def push_value = value ^^ { case x => s.push(x) }
    def value = val_of_var | wholeNumber ^^ { _.toInt }
    def val_of_var = ident ^^ { case x => st(x).toInt }

    /* (?s) add s modifier to the regex engine => Dot matches newline characters*/
    def body = """(?s)\[.*?\]""".r ^^ { case x => x.substring(1, x.length - 1) }
}

object ArnoldCEvaluator {
    def main(args: Array[String]) = {
        val p = new ArnoldCParser(new Stack[Int](), new HashMap[String, Int]())
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