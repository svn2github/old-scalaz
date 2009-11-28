package scalaz

sealed trait MAB[M[_, _], A, B] {
  val v: M[A, B]

  def :->[D](g: B => D)(implicit b: Bifunctor[M]) = b.bimap(v, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]) = b.bimap(v, f, identity[B])

  def ⋙[C](k: M[B, C])(implicit c: Category[M]) = c compose (k, v)

  def ⋘[C](k: M[C, A])(implicit c: Category[M]) = c compose (v, k)
}

trait MABs {
  implicit def mab[M[_, _], A, B](a: M[A, B]) = new MAB[M, A, B] {
    val v = a
  }
}
