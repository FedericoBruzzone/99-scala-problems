// scalac -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar ./AirlineFood.scala && scala -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar AirlineFoodEvauator helloworld.alf

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.io._

class AirlineFoodParser(var s: Stack[Int]) extends JavaTokenParsers {
  def program = stmts

  def stmts = rep(stmt)
  def stmt = init | init_and_point | p_down | p_up | set_p | add_to_p_loc | sub_to_p_loc | mul_to_p_loc | jump_to | jump_form | stdin | stdout
  def init = "You" ~> "ever" ~> "notice" ~> vari <~ "?"
  def init_and_point = "What's" ~> "the" ~> "deal" ~> "with" ~> vari <~ "?"
  def p_down = "Um,"
  def p_up = "Yeah,"
  def set_p = "Let's" ~> "talk" ~> "about" ~> vari <~ "."
  def add_to_p_loc = "It's" ~> "kinda" ~> "like" ~> vari <~ "."
  def sub_to_p_loc = "Not" ~> "like" ~> vari <~ "."
  def mul_to_p_loc = "Just" ~> "like" ~> vari <~ "."
  def jump_to = "So..."
  def jump_form = "Moving on..."
  def stdin = "Right?"
  def stdout = "See?"

  def vari = """[^\?!\.]+""".r 
}

object AirlineFoodEvauator {
  def main(args: Array[String]) {
    val p = new AirlineFoodParser(new Stack[Int]())
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
