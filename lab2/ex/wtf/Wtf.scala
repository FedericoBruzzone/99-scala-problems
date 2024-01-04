// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./Wtf.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar WtfEvaluator ops.wtf
import scala.util.parsing.combinator._
import scala.collection.mutable._


class WtFCombinators(var the_stack: Stack[Int],
                     var the_table: HashMap[Char,(Int, String)],
                     var args_table: Array[Int]) extends JavaTokenParsers {
  def wtf_program = def_sect ~ wtf_body ^^ {_ => (the_stack,the_table)}
  def def_sect = rep(adef)
  def wtf_body = expr ~ rep(expr)
  def adef = "def" ~> fun_name ~ args ~ ("=" ~> """.*\n""".r ) ^^ {
    case c ~ n ~ s => the_table(c) = (n, s.dropRight(1));
  }
  def fun_name = """[A-Z]""".r ^^ { s => s.charAt(0) }
  def block = """\[.*?\]""".r ^^ {s => s.substring(1,s.length-1)}
  def args = decimalNumber ^^ { n => n.toInt }
  def expr: Parser[Any] = (intexpr | varexpr | fun_call | if_expr
    | unop ^^ { (f:(Int => Int)) => the_stack.push(f(the_stack.pop)) }
    | "!" ^^ { _ => println(the_stack.pop) })
  def intexpr = "0" ^^ { _ => the_stack.push(0) }
  def unop = (
    "-" ^^ { _ => (a: Int) => a-1 } | "+" ^^ { _ => (a: Int) => a+1 }
  )
  def if_expr = "?" ~> block ~ (":" ~> block) ^^ {
  case b1 ~ b2 =>
    if (the_stack.pop == 0) parseAll(wtf_body, b1) else parseAll(wtf_body, b2)
  }
  def varexpr = "$" ~> decimalNumber ^^ {
    n => the_stack.push(args_table(n.toInt)) }
  def fun_call = fun_name ^^ { c =>
    val argc = the_table(c)._1
    var local_args_table = new Array[Int](10)
    argc to 1 by -1 foreach( n => local_args_table(n) = the_stack.pop )
    val p1 = new WtFCombinators(the_stack, the_table, local_args_table)
    p1.parseAll(p1.wtf_body, the_table(c)._2)
  }
}

object WtfEvaluator {
  def main(args: Array[String]) = {
    val p = new WtFCombinators( new Stack[Int](), new HashMap[Char,(Int,String)](), new Array[Int](10))
    args.foreach { filename =>
      val src = scala.io.Source.fromFile(filename)
      val lines = src.mkString
      p.parseAll(p.wtf_program, lines) match {
        case p.Success((s,t),_) => println(s)
        println("Symbol Table :-")
        t foreach { m => println(m) } case x => print(x.toString)
      }
      src.close()
    }
  }
}
