// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./LogLang3.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar LogLangEvaulator test.ll
import scala.util.parsing.combinator._
import java.io._

class LogLangParser extends JavaTokenParsers {
  var i = 1

  def program = tasks
  def tasks = rep(task)
  def task = "task" ~> ident ~ block ^^ { case name ~ body => i = 1; println("Task " + name); parseAll(stmts, body) }
  def block = """(?s)\{.*?\}""".r ^^ { case s => s.substring(1, s.length - 1) }

  def stmts = rep(stmt)
  def stmt = (remove | rename | backup | merge) ^^ { case x => print("  [op" + i + "] "); println(x); i += 1 }
  def remove = "remove" ~> fn ^^ { case (x: String) => { new File(x).delete() } }
  def rename = "rename" ~> fn ~ fn ^^ { case (x: String) ~ (y: String) => { new File(x).renameTo(new File(y)) } }
  def backup = "backup" ~> fn ~ fn ^^ { case (x: String) ~ (y: String) => {
        try { new FileOutputStream(y).getChannel().transferFrom(new FileInputStream(x).getChannel(), 0, Long.MaxValue); true }
        catch { case e: Exception => false }
    }
  }
  def merge = "merge" ~> fn ~ fn ~ fn  ^^ { case (x: String) ~ (y: String) ~ (z: String) =>
    var m = new FileOutputStream(z, true);
    try {
        m.getChannel().transferTo(0, Long.MaxValue, new FileOutputStream(x).getChannel());
        m.getChannel().transferTo(0, Long.MaxValue, new FileOutputStream(y).getChannel());
        true
    } catch { case e: Exception => false }
  }

  def fn = stringLiteral ^^ { case s => s.substring(1, s.length - 1) }
}

object LogLangEvaulator {
    def main(args: Array[String]) {
      val p = new LogLangParser
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
