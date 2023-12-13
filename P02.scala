// (*) Find the last but one element of a list.

object P02 {
  def lastButOne[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException
      case x :: Nil => throw new NoSuchElementException
      case x :: y :: Nil => x
      case x :: xs => lastButOne(xs)
    }
  }

  def main(args: Array[String]) {
    println(lastButOne(List(1, 1, 2, 3, 5, 8)))
  }
}
