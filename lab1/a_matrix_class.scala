class Matrix (val rows: Int, val cols: Int) extends AnyRef {
  private val mat = new Array[Array[Int]](rows)
  for (i <- 0 until rows)
    mat(i) = new Array[Int](cols)

  def apply(row: Int, col: Int) = mat(row)(col)
  def update(row: Int, col: Int, value: Int) = mat(row)(col) = value
  override def toString = mat.map(_.mkString(" ")).mkString("\n")

  // matrix equivalence
  def ~= (that: Matrix) = {
    if (this.rows != that.rows || this.cols != that.cols)
      false
    this.mat.zip(that.mat).forall { case (a, b) => a.sameElements(b) }
  }

  // matrix  copy
  def copy = {
    val m = new Matrix(rows, cols)
    for (i <- 0 until rows; j <- 0 until cols)
      m(i, j) = this(i, j)
    m
  }

  // matrix addition
  def + (that: Matrix) = {
    val m = new Matrix(rows, cols)
    for (i <- 0 until rows; j <- 0 until cols)
      m(i, j) = this(i, j) + that(i, j)
    m
  }

  // scalar-matrix multiplication
  def * (scalar: Int) = {
    val m = new Matrix(rows, cols)
    for (i <- 0 until rows; j <- 0 until cols)
      m(i, j) = this(i, j) * scalar
    m
  }

  // matrix-matrix multiplication
  def * (that: Matrix) = {
    val m = new Matrix(rows, that.cols)
    for (i <- 0 until rows; j <- 0 until that.cols) {
      var sum = 0
      for (k <- 0 until cols)
        sum += this(i, k) * that(k, j)
      m(i, j) = sum
    }
    m
  }

  // matrix transpose
  def t = {
    val m = new Matrix(cols, rows)
    for (i <- 0 until rows; j <- 0 until cols)
      m(j, i) = this(i, j)
    m
  }

  // matrix norm (matrix 1-norm)
  def norm = {
    var max = 0
    for (i <- 0 until rows) {
      var sum = 0
      for (j <- 0 until cols)
        sum += this(i, j)
      if (sum > max) max = sum
    }
    max
  }

}
object Matrix {
  def apply(rows: Int, cols: Int): Matrix = new Matrix(rows, cols)
}

object Test extends App {
  val m = Matrix(2, 3)
  m(1, 1) = 7
  val m2 = Matrix(2, 3)
  m2(1, 1) = 6

  println(m)
  println(m ~= m2)
  println(m + m2)
  println(m * 2)
  println(m * m2.t)
  println(m.norm)

}
