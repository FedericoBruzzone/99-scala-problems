class /*Simple*/Editor(val _buffer: String) {
  var buffer: List[Char] = _buffer.toList
  var cursor: Int = 0

  def x = {
    if ((cursor < buffer.length) && (buffer.apply(cursor) != ' ')) {
      buffer = buffer.take(cursor) ++ buffer.drop(cursor + 1)
    }
  }

}

object Main extends App {
  val editor = new Editor("bleh")
  editor.x
  println(editor.buffer)
}
