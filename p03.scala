// (*) Find the Kth element of a list.

object P03 {
  def nth[A](n: Int, list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException
      case h :: t => if (n == 0) h else nth(n - 1, t)
    }
  }

  def main(args: Array[String]) = {
    println(nth(2, List(1, 1, 2, 3, 5, 8)))
  }
}

