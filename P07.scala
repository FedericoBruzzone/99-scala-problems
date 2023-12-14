// (**) Flatten a nested list structure.

object P07 {
  def flatten(list: List[Any]): List[Any] = {
    // list flatMap {
    //   case l: List[_] => flatten(l)
    //   case e => List(e)
    // }
    list match {
      case Nil => Nil
      case (l: List[_]) :: t => flatten(l) ::: flatten(t)
      case x :: t => x :: flatten(t)
    }
  }

  def main(args: Array[String]) {
    val l = List(List(1, 1), 2, List(3, List(5, 8)))
    println(flatten(l))
  }
}
