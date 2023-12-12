class IsPalindrome {
    def isPalindrome(s: String): Boolean = {
      val s2 = s.reverse;
      return s == s2;
    }
}

class IsAnAnagram {
  def isAnAnagram(s1: String, s2: String): Boolean = {
    s1.sorted == s2.sorted
  }
}

class Factors {
  def factors(n: Int): List[Int] = {
    (1 to n).filter(n % _ == 0).toList
  }
}

class IsProper(f: Factors = new Factors) {
  def isProper(n: Int): Boolean = {
    f.factors(n).sum - n == n
  }
}

object FunctionalScala {
  def main(args: Array[String]): Unit = {
    val ian = new IsAnAnagram()
    println(ian isAnAnagram("abc", "cba"))
    println(ian isAnAnagram("abc", "cb"))

    val ip = new IsPalindrome()
    println(ip isPalindrome("racecar"))
    println(ip isPalindrome("hello"))

    val f = new Factors()
    println(f factors 10)
    println(f factors 11)

    val ip2 = new IsProper()
    println(ip2 isProper 6)
  }
}

