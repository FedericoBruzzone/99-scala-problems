abstract trait UndoRedu { def undo(): Unit; def ctrlr(): Unit; def after_method(s: String): Unit }
abstract trait Debug { def cprintln(): Unit }

trait EditorDebug extends Debug {
  this: Editor =>
  var last_command: String = ""
  def cprintln() = {
    println(s"$cursor $last_command $buffer")
  }
}

trait EditorUndoRedu extends UndoRedu {
  this: Editor =>
  var history_buffer: Array[List[Char]] = Array()
  var history_last_command: Array[String] = Array()
  var history_cursor: Array[Int] = Array()
  var i = -1
  var has_effect = false

  def after_method(s: String) = {
    last_command = s
    i += 1
    this.cprintln()
    if (i >= history_buffer.length) {
      history_buffer = history_buffer :+ buffer
      history_last_command = history_last_command :+ last_command
      history_cursor = history_cursor :+ cursor
    }
    else {
      history_buffer(i) = buffer
      history_last_command(i) = last_command
      history_cursor(i) = cursor
    }
    has_effect = false
  }
  def undo() = {
    if (i > 0) {
      i -= 1
      buffer = history_buffer(i)
      last_command = history_last_command(i)
      cursor = history_cursor(i)
      print("[UNDO]: "); this.cprintln()
    }
    has_effect = true
  }
  def ctrlr() = {
    if (has_effect && (i < history_buffer.length - 1)) {
      i += 1
      buffer = history_buffer(i)
      last_command = history_last_command(i)
      cursor = history_cursor(i)
    }
    print("[REDO]: "); this.cprintln()
  }
}

// =========== Editor ===========

abstract trait IEditor {
  def x: Unit
  def dw: Unit
  def i(c: Char): Unit
  def set_cursor_position(pos: Int): Unit
}

class Editor(val _buffer: String) extends IEditor
with EditorUndoRedu
with EditorDebug {
  var buffer: List[Char] = _buffer.toList
  var cursor: Int = 0

  def x = {
    if ((cursor < buffer.length)) {
      buffer = buffer.take(cursor) ++ buffer.drop(cursor + 1)
    }
    after_method("x")
  }

  def dw : Unit = {
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
}

object Main extends App {
  val editor = new Editor("hello how are you?")
  editor.set_cursor_position(5)
  editor.x
  editor.dw
  editor.i('!')
  editor.undo
  editor.ctrlr
  editor.undo
  editor.undo
  editor.i('!')
  editor.ctrlr
}
