// scala -cp .:./scala-parser-combinators_2.13-2.3.0.jar ./brainfuck.scala ./1_brainfuck.input ./2_brainfuck.input ./3_brainfuck.input
// import scala.collection.mutable._
import scala.util.parsing.combinator._
import scala.io.StdIn._

trait Command
case class IncrementDP() extends Command
case class DecrementDP() extends Command
case class IncrementByteDP() extends Command
case class DecrementByteDP() extends Command
case class PrintByteDP() extends Command
case class ReadByteDP() extends Command
case class CommandList(l: List[Any]) extends Command

class TestParser extends JavaTokenParsers {
  val arr: Array[Int] = new Array[Int](30000)
  var p: Int = 0

  def program = statement_list ^^ { case l => Run(l) }
  def statement_list: Parser[CommandList] = rep(statement) ^^ { case l => CommandList(l) }
  def operator = (">" | "<" | "+" | "-" | "." | ",") ^^ {
      case ">" => IncrementDP
      case "<" => DecrementDP
      case "+" => IncrementByteDP
      case "-" => DecrementByteDP
      case "." => PrintByteDP
      case "," => ReadByteDP
  }
  def spurious_str: Parser[String] = """[^><\+\-\[\]\.#,]+""".r ^^ { _ => "" } // def spurious_str: Parser[Any] = """[\w\s"';!\?\^:]+""".r ^^ { _ => "" }
  def statement = (operator
                  | "[" ~> statement_list <~ "]"
                  | spurious_str)

  def incrementDP() = { p = p + 1 }
  def decrementDP() = { p = p - 1 }
  def incrementByteDP() = { arr(p) = arr(p) + 1 }
  def decrementByteDP() = { arr(p) = arr(p) - 1 }
  def printByteDP() = { println(arr(p).toChar) }
  def readByteDP() = { arr(p) = readInt }

  def Run(l: CommandList): Unit = l.l.foreach {
    case IncrementDP => incrementDP()
    case DecrementDP => decrementDP()
    case IncrementByteDP => incrementByteDP()
    case DecrementByteDP => decrementByteDP()
    case PrintByteDP => printByteDP()
    case ReadByteDP => readByteDP()
    case l: CommandList => while (arr(p) != 0) Run(l)
    case _ => ()
  }
}

object TestMain {
  def main(args: Array[String]) = {
    val p = new TestParser
    args.foreach { filename =>
      val src = scala.io.Source.fromFile(filename)
      val lines = src.mkString
      p.parseAll(p.program, lines) match {
        case p.Success(result, _) => result
        case x => print(x.toString)
      }
      src.close()
    }
  }
}


// class TestParser extends JavaTokenParsers {
//   // override protected val whiteSpace = """[^><\+\-\[\]\.#,]*""".r
//   val arr: Array[Int] = new Array[Int](30000)
//   var p: Int = 0
//
//   def program = statement_list ^^ { case l => Run(l) }
//   def statement_list: Parser[List[Any]] = rep(statement) ^^ { case l => l }
//   def statement = (operator | "[" ~> statement_list <~ "]" | spurious_str)
//   def spurious_str: Parser[String] = """[^><\+\-\[\]\.#,]+""".r ^^ { _ => "" } // def spurious_str: Parser[Any] = """[\w\s"';!\?\^:]+""".r ^^ { _ => "" }
//
//   def operator = (">" | "<" | "+" | "-" | "." | ",") ^^ {
//       case ">" => IncrementDP
//       case "<" => DecrementDP
//       case "+" => IncrementByteDP
//       case "-" => DecrementByteDP
//       case "." => PrintByteDP
//       case "," => ReadByteDP
//   }
//
//   def incrementDP() = { p = p + 1 }
//   def decrementDP() = { p = p - 1 }
//   def incrementByteDP() = { arr(p) = arr(p) + 1 }
//   def decrementByteDP() = { arr(p) = arr(p) - 1 }
//   def printByteDP() = { print(arr(p).toChar) }
//   def readByteDP() = { arr(p) = readInt }
//
//   def Run(l: List[Any]): Unit = l.foreach {
//     case IncrementDP => incrementDP()
//     case DecrementDP => decrementDP()
//     case IncrementByteDP => incrementByteDP()
//     case DecrementByteDP => decrementByteDP()
//     case PrintByteDP => printByteDP()
//     case ReadByteDP => readByteDP()
//     case l: List[Any] => while (arr(p) != 0) Run(l)
//     case _ => ()
//   }
// }
//
// object TestMain {
//   def main(args: Array[String]) = {
//     val p = new TestParser
//     args.foreach { filename =>
//       val src = scala.io.Source.fromFile(filename)
//       val lines = src.mkString
//       p.parseAll(p.program, lines) match {
//         case p.Success(result, _) => result
//         case x => print(x.toString)
//       }
//       src.close()
//     }
//   }
// }

