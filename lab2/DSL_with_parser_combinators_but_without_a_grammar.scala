// scalac -classpath "./scala-parser-combinators_2.13-2.3.0.jar" DSL_with_parser_combinators_but_without_a_grammar.scala && scalac -classpath "./scala-parser-combinators_2.13-2.3.0.jar" test.scala
import scala.util.parsing.combinator._

class TestParser extends JavaTokenParsers {
  var map1 = Map("+" -> ((_:Int) + (_:Int)),
                 "-" -> ((_:Int) - (_:Int)),
                 "*" -> ((_:Int) * (_:Int)),
                 "/" -> ((_:Int) / (_:Int)))

  var map2 = Map("sqrt" -> (scala.math.sqrt(_)),
                  "sin" -> (scala.math.sin(_)),
                  "cos" -> (scala.math.cos(_)),
                  "tan" -> (scala.math.tan(_)))

  // def expr: Parser[Any] = fact ~ opt(op ~ fact)
  def expr = (fact ~ op ~ fact) ^^ { case t1 ~ opf ~ t2 => opf.get(t1, t2) } | fact
  def op = ("*" | "/") ^^ { map1.get(_) }
  def fact = (term ~ op2 ~ term) ^^ { case t1 ~ opf ~ t2 => opf.get(t1, t2) } | term
  def op2 = ("+" | "-") ^^ { map1.get(_) }
  def term: Parser[Int] = ("(" ~> expr <~ ")") ^^ { _.toInt } | decimalNumber ^^ { _.toInt } | fun ^^ { _.toInt }
  def fun = {
    ("sqrt" ~> "(" ~> expr <~ ")") ^^ { map2.get("sqrt").get(_) } |
    ("sin" ~> "(" ~> expr <~ ")") ^^ { map2.get("sin").get(_) } |
    ("cos" ~> "(" ~> expr <~ ")") ^^ { map2.get("cos").get(_) } |
    ("tan" ~> "(" ~> expr <~ ")") ^^ { map2.get("tan").get(_) }
  }
}

object TestMain {
  def main(args: Array[String]) = {
    // val lines = scala.io.Source.fromFile(args(0)).mkString
    val parser = new TestParser
    val result = parser.parseAll(parser.expr, "sqrt(4) + 100")
    println(result)
  }
}


