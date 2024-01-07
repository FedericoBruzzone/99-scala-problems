// scalac -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar ./Calculator.scala && scala -cp .:./../lab2/scala-parser-combinators_2.13-2.3.0.jar CEvaluator hello_world.calc

import scala.util.parsing.combinator._
import scala.io._

class CParsers extends JavaTokenParsers {
  def prog = stmts

  def stmts = repsep(stmt, ";") <~ opt(";")
  def stmt = (output
             | output_ascii
             | add
             | sub
             | mul
             | div
             | mod
             | assign
             | clearOutput
             | stopsProgram
             | jump_to_label
             | if_zero
             | if_one
             | if_eq
             | if_not_eq
             | boh
             )
  def boh = my_ident <~ "." <~ "?"

  def add = (my_ident <~ ".") ~ term <~ "." <~ "+"
  def sub = (my_ident <~ ".") ~ term <~ "." <~ "-"
  def mul = (my_ident <~ ".") ~ term <~ "." <~ "*"
  def div = (my_ident <~ ".") ~ term <~ "." <~ "/"
  def mod = (my_ident <~ ".") ~ term <~ "." <~ "%"
  def assign = (my_ident <~ ".") ~ term <~ "." <~ "v"
  def clearOutput = ".!"
  def stopsProgram = ".~"
  def output = my_ident <~ "." <~ "#"
  def output_ascii = my_ident <~ "." <~ "@"
  def jump_to_label = my_ident <~ "." <~ "^"
  def if_zero = (my_ident <~ ".") ~ (my_ident <~ ".") ~ my_ident <~ "." <~ "!" <~ "0"
  def if_one = (my_ident <~ ".") ~ (my_ident <~ ".") ~ my_ident <~ "." <~ "!" <~ "1"
  def if_eq = (my_ident <~ ".") ~ (my_ident <~ ".") ~ (my_ident <~ ".") ~ my_ident <~ "." <~ "!" <~ "="
  def if_not_eq = (my_ident <~ ".") ~ (my_ident <~ ".") ~ (my_ident <~ ".") ~ my_ident <~ "." <~ "!" <~ "=" <~ "="

  def term = stdIn | my_ident | wholeNumber
  def stdIn = "$"

  def my_ident = """[^\.]+""".r
}

object CEvaluator {
  def main(args: Array[String]): Unit = {
    val p = new CParsers
    args.foreach { arg =>
      val source = Source.fromFile(arg)
      val input = source.mkString
      p.parseAll(p.prog, input) match {
        case p.Success(r, n) => println(r)
        case x => println(x)
      }
      source.close ()
    }
  }
}

