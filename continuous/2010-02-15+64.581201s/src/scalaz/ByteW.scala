package scalaz

sealed trait ByteW extends PimpedType[Byte] {
  import Scalaz._
  
  def ∏ = multiplication(value)
}

trait Bytes {
  implicit def ByteTo(n: Byte): ByteW = new ByteW {
    val value = n
  }
}
