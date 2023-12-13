// (*) Find the last element of a list.

object P01 {
  def last[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case x :: xs => last(xs)
  }

  def main(args: Array[String]) {
    println(P01.last(List(1, 1, 2, 3, 5, 8)))
  }
}

