class Monoid[T](s: Set[T], add: (T, T) => T, identity: T) {
  def isClosed: Boolean = {
    s.forall { x =>
      s.forall { y =>
        s.contains(add(x, y))
      }
    }
  }

  def isAssociative: Boolean = {
    s.forall { x =>
      s.forall { y =>
        s.forall { z =>
          add(add(x, y), z) == add(x, add(y, z))
        }
      }
    }
  }

  def hasIdentity: Boolean = {
    s.forall { x =>
      add(x, identity) == x && add(identity, x) == x
    }
  }

  def isMonoid: Boolean = {
    isClosed && isAssociative && hasIdentity
  }
}
object Monoid {
  def apply[T](s: Set[T], add: (T, T) => T, identity: T): Monoid[T] = {
    new Monoid(s, add, identity)
  }
}

class Group[T](s: Set[T], add: (T, T) => T, identity: T, inverse: T => T) extends Monoid(s, add, identity) {
  def hasInverse: Boolean = {
    s.forall { x =>
      s.contains(inverse(x))
    }
  }

  def isGroup: Boolean = {
    isMonoid && hasInverse
  }
}
object Group {
  def apply[T](s: Set[T], add: (T, T) => T, identity: T, inverse: T => T): Group[T] = {
    new Group(s, add, identity, inverse)
  }
}

class Ring[T](s: Set[T], add: (T, T) => T, identity: T, inverse: T => T, multiply: (T, T) => T, zero: T) extends Group(s, add, identity, inverse) {
  def isDistributive: Boolean = {
    s.forall { x =>
      s.forall { y =>
        s.forall { z =>
          multiply(x, add(y, z)) == add(multiply(x, y), multiply(x, z)) &&
          multiply(add(y, z), x) == add(multiply(y, x), multiply(z, x))
        }
      }
    }
  }

  def isRing: Boolean = {
    isGroup && isDistributive
  }
}
object Ring {
  def apply[T](s: Set[T], add: (T, T) => T, identity: T, inverse: T => T, multiply: (T, T) => T, zero: T): Ring[T] = {
    new Ring(s, add, identity, inverse, multiply, zero)
  }
}

object playing_around_with_algebra {
  def main(args: Array[String]): Unit = {
    val m1 = Monoid(Set(true, false), (x: Boolean, y: Boolean) => x || y, false)
    println(m1.isMonoid)

    val m2 = Monoid(Set(0, 1, 2, 3, 4), (x: Int, y: Int) => (x + y) % 5, 0)
    println(m2.isMonoid)

    val g1 = Group(Set(true, false), (x: Boolean, y: Boolean) => x || y, false, (x: Boolean) => !x)
    println(g1.isGroup)

    val g2 = Group(Set(0, 1, 2, 3), (x: Int, y: Int) => (x * y) % 4, 1, (x: Int) => (4 - x) % 4)
    println(g2.isGroup)

    val r1 = Ring(Set(0, 1, 2, 3, 4), (x: Int, y: Int) => (x + y) % 5, 0, (x: Int) => (5 - x) % 5, (x: Int, y: Int) => (x * y) % 5, 0)
    println(r1.isRing)
  }
}
