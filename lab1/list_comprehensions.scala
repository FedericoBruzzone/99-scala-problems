class SquaredNumbers {
  // def square_numbers(l: List[Any]): List[Any] = {
  //   (for (x <- l) yield x match {
  //     case i: Int => i * i
  //     case d: Double => d * d
  //     case f: Float => f * f
  //     case t: Tuple2[Any, Any] => square_numbers(t.productIterator.toList)
  //     case t: Tuple3[Any, Any, Any] => square_numbers(t.productIterator.toList)
  //     case l: List[Any] => square_numbers(l)
  //     case _ => ""
  //   }).filter(_ != "")
  // }
  def square_numbers(inputs: List[Any]): List[Any] = {
    return for (input @ (_: Int | _: List[Any]) <- inputs) yield input match {
      case l: List[Any] =>
        square_numbers(l)
      case num: Int =>
        num * num
    }
  }
}

class Intersect {
  def intersect(l1: List[Any], l2: List[Any]): List[Any] = {
    // for { i <- l1 j <- l2 if i == j  } yield i
    for(i <- l1; j <- l2; if i == j) yield i
  }
}

class SymmetricDifference {
  def symmetric_difference(l1: List[Any], l2: List[Any]): List[Any] = {
    (for(i <- l1; if !l2.contains(i)) yield i) ::: (for(i <- l2; if !l1.contains(i)) yield i)
  }
}

object ListComprehensions {
  def main(args: Array[String]) {
    val sq = new SquaredNumbers()
    println(sq.square_numbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5,7,'a') :: Nil))

    val i = new Intersect()
    println(i.intersect(1 :: 2 :: 3 :: 4 :: Nil, 2 :: 4 :: 6 :: 8 :: Nil))

    val sd = new SymmetricDifference()
    println(sd.symmetric_difference(List(1,2,3,4,5), List(4,5,6,7,8)))
  }
}
