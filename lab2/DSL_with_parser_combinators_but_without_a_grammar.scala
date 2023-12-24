// scalac -classpath "./scala-parser-combinators_2.13-2.3.0.jar" DSL_with_parser_combinators_but_without_a_grammar.scala && scalac -classpath "./scala-parser-combinators_2.13-2.3.0.jar" test.scala
// scala -cp .:./scala-parser-combinators_2.13-2.3.0.jar DSL_with_parser_combinators_but_without_a_grammar.scala
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

  var variables = Map[String, Int]()

  // def expr: Parser[Any] = fact ~ opt(op ~ fact)
  def start = assign | expr

  def assign: Parser[Int] = (id ~ "=" ~ assign) ^^ { case v ~ "=" ~ e => variables += (v -> e); e } | expr
  def id = """[a-zA-Z_]\w*""".r

  def expr: Parser[Int] = (fact ~ op ~ expr) ^^ { case t1 ~ opf ~ t2 => opf.get(t1, t2) } | fact
  def op = ("+" | "-") ^^ { map1.get(_) }

  def fact: Parser[Int] = (term ~ op2 ~ fact) ^^ { case t1 ~ opf ~ t2 => opf.get(t1, t2) } | term
  def op2 = ("*" | "/") ^^ { map1.get(_) }

  def term: Parser[Int] = (("(" ~> expr <~ ")") ^^ { _.toInt }
                           | decimalNumber ^^ { _.toInt }
                           | "-" ~> decimalNumber ^^ { -1 * _.toInt }
                           | fun ^^ { _.toInt })
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
    val r = parser.parseAll(parser.expr, "1 * 1 * 1")
    println(r)
    val r2 = parser.parseAll(parser.expr, "1 + 1 + 1")
    println(r2)
    val result = parser.parseAll(parser.expr, "sqrt(4) + 100 * 2")
    println(result)
    val result2 = parser.parseAll(parser.assign, "x = 100")
    // println(parser.variables)
    val result3 = parser.parseAll(parser.start, "x = y = 1 + 1")
    println(result3)
    // println(parser.variables)
    val result4 = parser.parseAll(parser.start, "-1 * (1 + sqrt(4) + 100 * 2)")
    println(result4)
  }
}


