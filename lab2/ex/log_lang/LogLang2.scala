// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./LogLang2.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar LogLangEvaulator test.ll
import scala.util.parsing.combinator._
import java.io.{File, FileInputStream, FileOutputStream}
//import java.nio._

class LogLangParser extends JavaTokenParsers {
    var i = 1

    def program = tasks
    def tasks = rep1(task)
    def task = "task" ~> ident ~ ("{" ~> statements <~ "}") ^^ { case n ~ s => i = 1; println("Task " + n); s() } 

    def statements: Parser[() => Any] = rep(statement) ^^ { case l => () => l.foreach(a => a()) }
    def statement = (remove | rename | backup | merge) ^^ { case x => () => print("  [op" + i + "] "); println(x()); i += 1 }

    def remove: Parser[() => Any] = "remove" ~> file_name ^^ { case x => () => { new File(x).delete() } }
    def rename: Parser[() => Any] = "rename" ~> file_name ~ file_name ^^ { case x ~ y => () => { new File(x).renameTo(new File(y)) } }
    def backup: Parser[() => Any] = "backup" ~> file_name ~ file_name ^^ { case x ~ y => () => {
            try {
                new FileInputStream(x).getChannel().transferTo(0, Long.MaxValue, new FileOutputStream(x).getChannel())
            } catch {
                false
            }
            true
        } 
    }
    def merge: Parser[() => Any] = "merge" ~> file_name ~ file_name ~ file_name ^^ { case x ~ y ~ z => () => () }

    def file_name = stringLiteral ^^ { s => s.substring(1, s.length - 1) }
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