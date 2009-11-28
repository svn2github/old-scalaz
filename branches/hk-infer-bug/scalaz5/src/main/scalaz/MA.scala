package scalaz

trait Test[A]

sealed trait MA[M[_], A] {
  val v: M[A]

  def foo(implicit a: List[A]) = 0

}

trait MAs {
  implicit def ma[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val v = a
  }
}
