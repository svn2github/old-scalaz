package scalaz

trait Plus[P[_]] {
  def plus[A](a1: P[A], a2: => P[A]): P[A]
}

object Plus {
  implicit val ListPlus: Plus[List] = new Plus[List] {
    def plus[A](a1: List[A], a2: => List[A]) = a1 ::: a2
  }
}
