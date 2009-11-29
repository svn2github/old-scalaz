package scalaz

trait Each[-E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
}

object Each {
  implicit val IterableEach: Each[Iterable] = new Each[Iterable] {
    def each[A](e: Iterable[A], f: A => Unit) = e foreach f
  }
}

