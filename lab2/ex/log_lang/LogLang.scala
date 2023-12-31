// scalac -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar ./LogLang.scala && scala -cp .:./../../scala-parser-combinators_2.13-2.3.0.jar LogLangEvaulator test.ll
import scala.util.parsing.combinator._
import java.io.{File, FileInputStream, FileOutputStream}

sealed trait Operation { def run(): Boolean }

case class Remove(file_name: String) extends Operation { def run(): Boolean = {
    new File(file_name).delete()
  }
}
case class Rename(file_name: String, new_file_name: String) extends Operation { def run(): Boolean = {
    new File(file_name).renameTo(new File(new_file_name))
  }
}
case class Backup(file_name: String, backup_file_name: String) extends Operation { def run(): Boolean = {
    try {
      new FileOutputStream(backup_file_name).getChannel().transferFrom(new FileInputStream(file_name).getChannel(), 0, Long.MaxValue)
      true
    } catch {
      case e: Exception => false
    }
  }
}
case class Merge(file_name: String, file_name_2: String, new_file_name: String) extends Operation { def run(): Boolean = {
    try {
      val merged = new FileOutputStream(new_file_name, true)
      merged.write(new FileInputStream(file_name).readAllBytes())
      merged.write(new FileInputStream(file_name_2).readAllBytes())
      true
    } catch {
      case e: Exception => false
    }
  }
}

case class Task(name: String, operations: List[Operation]) {
  def run() {
    println("Task " + name)
    operations.zipWithIndex.foreach { case (operation, index) =>
      print("  [OP" + (index + 1) + "] ")
      val result = operation.run()
      println(result)
    }
  }
}

class LogLangParser extends JavaTokenParsers {
  def program = tasks ^^ { case t => t.foreach(_.run()) }

  def tasks = rep(task)
  def task = "task" ~> task_name ~ ("{" ~> rep(operations) <~ "}") ^^ { case name ~ ops => Task(name, ops) }

  def operations = remove | rename | backup | merge
  def remove = "remove" ~> "\"" ~> file_name <~ "\"" ^^ { case file_name => Remove(file_name) }
  def rename = "rename" ~> ("\"" ~> file_name <~ "\"") ~ ("\"" ~> file_name <~ "\"") ^^ { case file_name ~ new_file_name => Rename(file_name, new_file_name) }
  def backup = "backup" ~> ("\"" ~> file_name <~ "\"") ~ ("\"" ~> file_name <~ "\"") ^^ { case file_name ~ backup_file_name => Backup(file_name, backup_file_name) }
  def merge = "merge" ~> ("\"" ~> file_name <~ "\"") ~ ("\"" ~> file_name <~ "\"") ~ ("\"" ~> file_name <~ "\"") ^^ { case file_name ~ file_name_2 ~ new_file_name => Merge(file_name, file_name_2, new_file_name) }

  def task_name = """[a-zA-Z0-9]+""".r
  def file_name = """[^"]+""".r // def file_name = """[a-zA-Z0-9.]+""".r
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
