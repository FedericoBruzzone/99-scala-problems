// (*) Find out whether a list is a palindrome.

object P06 {
  def isPalindrome[A](list: List[A]): Boolean = {
    list == list.reverse
  }

  def main(args: Array[String]) {
    val list = List(1, 2, 3, 2, 1)
    println(isPalindrome(list))
  }
}
