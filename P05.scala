// (*) Reverse a list.

object P05 {
  def reverse[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case h :: t => reverse(t) ::: List(h)
    }
  }

  def reverse_tr_helper[A](list: List[A], acc: List[A]): List[A] = {
    list match {
      case Nil => acc
      case h :: t => reverse_tr_helper(t, h :: acc)
    }
  }
  def reverse_tr[A](list: List[A]): List[A] = {
    reverse_tr_helper(list, Nil)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    println(reverse(list))
    println(reverse_tr(list))
  }
}

