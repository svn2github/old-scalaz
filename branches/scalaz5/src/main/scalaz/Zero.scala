package scalaz

trait Zero[+Z] {
  val zero: Z
}

trait Zeros {
  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  implicit def ListZero[A]: Zero[List[A]] = zero(Nil)

  implicit def StreamZero[A]: Zero[Stream[A]] = zero(Stream.empty)
  
  implicit val StringZero: Zero[String] = zero("")
}
