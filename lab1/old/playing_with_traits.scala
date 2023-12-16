trait /*abstract class*/ Printable { def cprintln(): Unit }
trait /*abstract class*/ AfterMethod { def after_method(s: String): Unit }

trait Debug extends AnyRef
            with Printable {
  this: Editor =>
  abstract override def cprintln() = {
    print(cursor + " " + last_command + " ")
    super.cprintln()
  }
}

trait UndoRedo extends AnyRef
               with AfterMethod {
  this: Editor =>
  var history: List[String] = List()

  abstract override def after_method(s: String) = {
    history = s :: history
    super.after_method(s)
  }

  def undo() = {
    history match {
      case Nil => ()
      case head :: tail => {
        history = tail
        println("undoing " + head)
      }
    }
  }

  def redo() = {
    history match {
      case Nil => ()
      case head :: tail => {
        history = head :: history
        println("redoing " + head)
      }
    }
  }
}

class Editor(val _buffer: String) extends AnyRef
                                  with Printable
                                  with AfterMethod {
  var buffer: List[Char] = _buffer.toList
  var cursor: Int = 0
  var last_command: String = ""


  def x = {
    if ((cursor < buffer.length)) {
      buffer = buffer.take(cursor) ++ buffer.drop(cursor + 1)
    }
    after_method("x")
  }

  def dw {
    val first = buffer.take(cursor)
    while ((buffer.apply(cursor) != ' ') &&
           (buffer.apply(cursor) != '\n')) {
      cursor += 1
    }
    val second = buffer.drop(cursor + 1)
    buffer = first ++ second
    after_method("dw")
  }

  def i(c: Char) = {
    buffer = buffer.take(cursor) ++ List(c) ++ buffer.drop(cursor)
    cursor += 1
    after_method("i")
  }

  def set_cursor_position(pos: Int): Unit = {
    cursor = pos
  }

  override def cprintln() = {
    println(buffer.toString)
  }

  override def after_method(s: String) = {
    last_command = s
    this.cprintln
  }
}

object Main extends App {
  val editor = new Editor("hello how are you?") with Debug with UndoRedo
  editor.set_cursor_position(5)
  editor.x
  // editor.cprintln
  editor.dw
  editor.i('!')
}
