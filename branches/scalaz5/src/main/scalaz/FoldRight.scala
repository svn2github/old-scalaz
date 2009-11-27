package scalaz

trait FoldRight[-F[_]] {
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
}

object FoldRight {
  implicit val ListFoldRight = new FoldRight[List] {
    def foldRight[A, B](t: List[A], b: B, f: (A, => B) => B) = IterableFoldRight.foldRight(t, b, f)
  }

  implicit val IterableFoldRight = new FoldRight[Iterable] {
    def foldRight[A, B](t: Iterable[A], b: B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
  }
}
