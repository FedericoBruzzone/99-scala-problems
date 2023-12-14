// (**) Pack consecutive duplicates of list elements into sublists.

object P09 {
  def pack[A](list: List[A]): List[List[A]] = {
    def packR[A](list: List[A], acc: List[A]): List[List[A]] = {
      list match {
        case Nil => List(acc)
        case x :: y :: t if x == y => packR(y :: t, x :: acc)
        case x :: t => (x :: acc) :: packR(t, List())
      }

    }
    packR(list, List())
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 1, 1, 2, 3, 3, 4, 5, 5, 5)
    println(pack(list))
  }
}
