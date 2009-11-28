package scalaz

object Each {
  implicit val IterableEach = new Each[Iterable] {
    def each[A](e: Iterable[A], f: A => Unit) = e foreach f
  }
}

trait Each[-E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
}
