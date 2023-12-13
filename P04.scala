// (*) Find the number of elements of a list.

object P04 {
  def length[A](list: List[A]): Int = {
    list match {
      case Nil => 0
      case _ :: t => 1 + length(t)
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    println(length(list))
  }
}
