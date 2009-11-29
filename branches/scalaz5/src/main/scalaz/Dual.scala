package scalaz

trait Dual[A] {
  val value : A
}

trait Duals {
  implicit def dual[A](a: A): Dual[A] = new Dual[A] {
    val value = a
  }
}
