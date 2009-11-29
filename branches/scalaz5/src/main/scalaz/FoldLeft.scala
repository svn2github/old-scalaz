package scalaz

trait FoldLeft[-F[_]] {
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val IterableFoldLeft: FoldLeft[Iterable] = new FoldLeft[Iterable] {
    def foldLeft[B, A](t: Iterable[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }
}
