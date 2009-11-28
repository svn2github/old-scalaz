package scalaz

sealed trait Zero[+Z] {
  val zero: Z
}

trait Zeros {
  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  implicit def ListZero[A]: Zero[List[A]] = zero(Nil)
}
