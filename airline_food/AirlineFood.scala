// scalac -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar ./AirlineFood.scala && scala -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar AirlineFoodEvauator helloworld.alf

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.io._

class AirlineFoodParser(var s: List[Tuple2[String, Int]], var p: Int) extends JavaTokenParsers {
  def program = stmts

  def stmts = rep(stmt)
  def stmt: Parser[Any] = init | init_and_point | p_down | p_up | set_p | add_to_p_loc | sub_to_p_loc | mul_to_p_loc | stdin | stdout | loop
  def init = "You" ~> "ever" ~> "notice" ~> vari <~ "?" ^^ { case v => s = s ::: List((v, 1)); }
  def init_and_point = "What's" ~> "the" ~> "deal" ~> "with" ~> vari <~ "?" ^^ { case v => s = s ::: List((v, 1)); p = s.length - 1; }
  def p_down = "Um," ^^ { case _ => if (p > 0) { p -= 1; } }
  def p_up = "Yeah," ^^ { case _ => if (p < s.length - 1) { p += 1; } }
  def set_p = "Let's" ~> "talk" ~> "about" ~> vari <~ "." ^^ { case v => p = get_index_of(v); }
  def add_to_p_loc = "It's" ~> "kinda" ~> "like" ~> vari <~ "." ^^ { case v => s = s.updated(p, (s(p)._1, s(p)._2 + get_value_of(v))) }
  def sub_to_p_loc = "Not" ~> "like" ~> vari <~ "." ^^ { case v => s = s.updated(p, (s(p)._1, s(p)._2 - get_value_of(v)))}
  def mul_to_p_loc = "Just" ~> "like" ~> vari <~ "." ^^ { case v => s = s.updated(p, (s(p)._1, s(p)._2 * get_value_of(v)))}
  def loop = "So..." ~> body <~ "Moving on..." ^^ { case b => while(s(p)._2 != 0) { parseAll(stmts, b) } }
  def stdin = "Right?" ^^ { case _ => s = s.updated(p, (s(p)._1, StdIn.readInt()))}
  def stdout = "See?" ^^ { case _ => print(s(p)._2) }

  def body = """(?s).*?(?=Moving on...)""".r ^^ { case b => b } // All until the next Moving on...

  def get_index_of(v: String): Int = { return s.indexWhere(_._1 == v) }
  def get_value_of(v: String): Int = { return s(get_index_of(v))._2 }

  def vari = """[^\?!\.]+""".r ^^ { case v => v }
}

object AirlineFoodEvauator {
  def main(args: Array[String]) {
    val p = new AirlineFoodParser(List(), 0)
    args.foreach { arg =>
      val source = scala.io.Source.fromFile(arg)
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
