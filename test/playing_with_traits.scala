trait Printable {
  def cprint()
}

trait Debug extends Printable {
  abstract override def cprint() = {
    println("Debugging...")
    super.cprint()
  }
}

class Editor(val _buffer: String) extends Printable {
  var buffer: List[Char] = _buffer.toList
  var cursor: Int = 0

  def x = {
    if ((cursor < buffer.length)) {
      buffer = buffer.take(cursor) ++ buffer.drop(cursor + 1)
    }
  }

  def dw {
    val first = buffer.take(cursor)
    while ((buffer.apply(cursor) != ' ') &&
           (buffer.apply(cursor) != '\n')) {
      cursor += 1
    }
    val second = buffer.drop(cursor + 1)
    buffer = first ++ second
  }

  def i(c: Char) = {
    buffer = buffer.take(cursor) ++ List(c) ++ buffer.drop(cursor)
    cursor += 1
  }

  def set_cursor_position(pos: Int): Unit = {
    cursor = pos
  }

  override def cprint() = {
    println(buffer.toString)
  }
}
object Editor {
  def apply(buffer: String) = new Editor(buffer) with Debug
}

object Main extends App {
  val editor = /*new*/ Editor("hello how are you?")
  editor.set_cursor_position(5)
  editor.x
  editor.cprint
  editor.dw
  editor.i('!')
  // println(editor.buffer)
}
