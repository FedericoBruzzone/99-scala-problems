// (*) Run-length encoding of a list.

object P10 {
  def encode[A](list: List[A]): List[(Int, A)] = {
    list match {
      case Nil => Nil
      case x :: xs => {
        val (packed, next) = list.span(_ == x)
        (packed.length, x) :: encode(next)
      }
    }
  }

  def main(args: Array[String]) {
    println(encode(List('a, 'a, 'a, 'b, 'c, 'c, 'a)))
  }
}
