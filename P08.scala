// (**) Eliminate consecutive duplicates of list elements.

object P08 {
  def compress[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case x :: y :: xs if x == y => compress(y :: xs)
      case x :: xs => x :: compress(xs)
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 1, 2, 3, 3, 3, 4, 5, 5)
    println(compress(list))
  }
}
