package scalaz

sealed trait MAB[M[_, _], A, B] {
  val v: M[A, B]

  def :->[D](g: B => D)(implicit b: Bifunctor[M]) = b.bimap(v, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]) = b.bimap(v, f, identity[B])
}

trait MABs {
  implicit def mab[M[_, _], A, B](a: M[A, B]) = new MAB[M, A, B] {
    val v = a
  }
}
