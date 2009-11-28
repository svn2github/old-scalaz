package scalaz

trait Empty[+E[_]] {
  def empty[A]: E[A]
}

object Empty {
  implicit val ListEmpty: Empty[List] = new Empty[List] {
    def empty[A] = Nil
  }
}
