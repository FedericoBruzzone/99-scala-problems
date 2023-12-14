
object Goldbach {
  def is_prime(n: Int): Boolean = {
    n match {
      case 1 => false
      case 2 => true
      case _ => !(2 to (n-1)).exists(x => n % x == 0)
    }
  }

  def goldbach(n: Int): (Int, Int) = {
    val primes = (1 to n).filter(is_prime)
    val pair = for {
      i <- primes
      j <- primes
      if (i + j == n)
    } yield (i, j)
    pair(0)
  }

  def goldbach_list(n: Int, m: Int): List[(Int, Int)] = {
    (n to m).filter(_ % 2 == 0).map(goldbach).toList
  }


  def main(args: Array[String]) {
    val n = 4
    val (i, j) = goldbach(n)
    println(s"$n = $i + $j")

    val m = 20
    val list = goldbach_list(n, m)
    println(list)

  }
}
