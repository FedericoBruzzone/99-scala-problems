# Scala Info

## Imports

```scala
import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.io._
```

## Main

### Readin from file

```scala
object TestEvaluator {
  def main(args: Array[String]) = {
    val p = new TestParser
    args.foreach { filename =>
      val src = scala.io.Source.fromFile(filename)
      val lines = src.mkString
      p.parseAll(p.program, lines) match {
        case p.Success(result, _) => println(result)
        case x => println(x.toString)
      }
      src.close()
    }
  }
}
```

### Reading from args

```scala
object TestEvaluator {
  def main(args: Array[String]) {
    val p = new TestParser
    args.foreach { arg =>
      p.parseAll(p.program, arg) match {
        case p.Success(t, _) => {
          var printer = new TestPrinter
          printer.pprint(t)
          println()
        }
        case e: p.NoSuccess => println(e)
      }
    }
  }
}
```

## Stdin

```scala
// from scala.io._
StdIn.readInt()
```

## Regex

### Whitespaces

```scala
override def skipWhitespace = false
override protected val whiteSpace = """[ \t\f]+""".r // default is """\s+""".r

// Skip comments
override protected val whiteSpace = """(\s|//.*)+""".r
override protected val whiteSpace = """(\s|#.*)+""".r

// Skip all characters except valid ones (Admit only valid characters)
override protected val whiteSpace = """[^><\+\-\[\]\.#,]*""".r
```

### Using or Not Using Regex

```scala
// ==========================================================
override def skipWhitespace = false

def line = repsep(<x>, ",") ^^ {s => s.dropRight(1)}
// ==========================================================

// ==========================================================
def stmts = repsep(stmt, ";") <~ opt(";") // Calculator

def body_one_line = """.*\n""".r ^^ {s => s.dropRight(1)} // WTF
def body =  """(?s)\[.*?\]""".r ^^ {s => s.substring(1,s.length-1)} // WTF

def vari = """[^\?!\.\n\r,]+""".r // If a varibale/field can contain spaces
// ==========================================================
```

## Default Values

**ident** = """[a-zA-Z_][a-zA-Z0-9_]*""".r

**wholeNumber** = """-?[0-9]+""".r

**decimalNumber** = """(\d+(\.\d*)?|\d*\.\d+)""".r

**stringLiteral** = ("\""+"""([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r

**floatingPointNumber** = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

```scala
def filename = stringLiteral ^^ {s => s.substring(1,s.length-1)}
```

## Files

```scala
import java.io._
import scala.io._

// Delete file
new File("filename").delete()

// Rename file
new File("filename").renameTo(new File("newfilename"))

// Copy file
new FileOutputStream(backup_file_name).getChannel().transferFrom(new FileInputStream(file_name).getChannel(), 0, Long.MaxValue)

// Merge two files
val merged = new FileOutputStream(new_file_name, true)
merged.write(new FileInputStream(file_name).readAllBytes())
merged.write(new FileInputStream(file_name_2).readAllBytes())

// Merge multiple files
val files = List("file1", "file2", "file3")
val out = new FileOutputStream("merged")
files.foreach { name =>
  val in = new FileInputStream(name)
  val bytes = Stream.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray
  in.close()
  out.write(bytes)
}

// Create file
new File("filename").createNewFile()

// Create file and write to it
val pw = new PrintWriter(new File("filename"))
pw.write("Hello, world")
pw.close()

// Read lines from file
val lines = Source.fromFile("filename").getLines.toList
```

