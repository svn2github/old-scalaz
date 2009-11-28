package scalaz

sealed trait LongW {
  val value: Long

  def digits: List[Digit] = {
    import Scalaz._

    if(value == 0) List(0L)
    else if(value < 10) List(value)
    else value % 10L :: (value / 10L digits)
  }
}

trait Longs {
  implicit def LongTo(n: Long): LongW = new LongW {
    val value = n
  }

  implicit def LongFrom(n: LongW): Long = n.value
}
