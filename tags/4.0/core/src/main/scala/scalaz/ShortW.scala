package scalaz

sealed trait ShortW {
  val value: Short

  def |*| = ShortMultiplication.multiplication(value)  
}

object ShortW {
  implicit def ShortTo(n: Short): ShortW = new ShortW {
    val value = n
  }

  implicit def ShortFrom(n: ShortW) = n.value
}
