package scalaz

sealed trait MA[M[_], A] {
  val v: M[A]

  import Scalaz._

  def ∘[B](f: A => B)(implicit t: Functor[M]) = t.fmap(v, f)

  def map[B](f: A => B)(implicit t: Functor[M]) = ∘(f)

  def >|[B](f: => B)(implicit t: Functor[M]) = ∘(_ => f)

  def ∙[B](f: B => A)(implicit t: Cofunctor[M]) = t.comap(v, f)

  def |<[B](f: => A)(implicit t: Cofunctor[M]) = ∙((_: B) => f)

  def ⊛[B](f: M[A => B])(implicit a: Apply[M]) = a(f, v)

  def <⊛>[B, C](b: M[B], z: (A, B) => C)(implicit t: Functor[M], a: Apply[M]) = a(t.fmap(v, z.curry), b)

  def <⊛>[B, C, D](b: M[B], c: M[C], z: (A, B, C) => D)(implicit t: Functor[M], a: Apply[M]) = a(a(t.fmap(v, z.curry), b), c)

  def <⊛>[B, C, D, E](b: M[B], c: M[C], d: M[D], z: (A, B, C, D) => E)(implicit t: Functor[M], a: Apply[M]) = a(a(a(t.fmap(v, z.curry), b), c), d)

  def <⊛>[B, C, D, E, F](b: M[B], c: M[C], d: M[D], e: M[E], z: (A, B, C, D, E) => F)(implicit t: Functor[M], a: Apply[M]) = a(a(a(a(t.fmap(v, z.curry), b), c), d), e)

  def ⊛>[B](b: M[B])(implicit t: Functor[M], a: Apply[M]) = <⊛>(b, (_, b: B) => b)

  def <⊛[B](b: M[B])(implicit t: Functor[M], a: Apply[M]) = <⊛>(b, (a, _: B) => a)

  def <<⊛>>[B](b: M[B])(implicit t: Functor[M], a: Apply[M]) = <⊛>(b, (_: A, _: B))

  def <<⊛>>[B, C](b: M[B], c: M[C])(implicit t: Functor[M], a: Apply[M]) = <⊛>(b, c, (_: A, _: B, _: C))

  def <<⊛>>[B, C, D](b: M[B], c: M[C], d: M[D])(implicit t: Functor[M], a: Apply[M]) = <⊛>(b, c, d, (_: A, _: B, _: C, _: D))

  def <<⊛>>[B, C, D, E](b: M[B], c: M[C], d: M[D], e: M[E])(implicit t: Functor[M], a: Apply[M]) = <⊛>(b, c, d, e, (_: A, _: B, _: C, _: D, _: E))

  def ↦[F[_], B](f: A => F[B])(implicit a: Applicative[F], t: Traverse[M]): F[M[B]] =
    t.traverse(f, v)

  def ∗[B](f: A => M[B])(implicit b: Bind[M]) = b.bind(v, f)

  def ∗|[B](f: => M[B])(implicit b: Bind[M]) = ∗(_ => f)

  def flatMap[B](f: A => M[B])(implicit b: Bind[M]) = ∗(f)

  def ⟴(z: => M[A])(implicit p: Plus[M]) = p.plus(v, z)

  def ➝:(a: A)(implicit p: Plus[M], q: Pure[M]) = p.plus(q.pure(a), v)

  def ➡(f: A => Unit)(implicit e: Each[M]) = e.each(v, f)

  def foreach(f: A => Unit)(implicit e: Each[M]) = ➡(f)

  def foldl[B](b: B, f: (B, A) => B)(implicit r: FoldLeft[M]) = r.foldLeft[B, A](v, b, f)

  def foldl1(f: (A, A) => A)(implicit r: FoldLeft[M]) = foldl[Option[A]](None, (a1, a2) => Some(a1 match {
    case None => a2
    case Some(x) => f(a2, x)
  }))

  def listl(implicit r: FoldLeft[M]) = {
    val b = new scala.collection.mutable.ListBuffer[A]
    foldl[scala.Unit]((), (_, a) => b += a)
    b.toList
  }

  def ∑(implicit r: FoldLeft[M], m: Monoid[A]) = foldl[A](m.zero, m append (_, _))

  def ♯(implicit r: FoldLeft[M]) = foldl[Int](0, (b, _) => b + 1)

  def len(implicit l: Length[M]) = l len v

  def max(implicit r: FoldLeft[M], ord: Order[A]) =
    foldl1((x: A, y: A) => if (x ≩ y) x else y)

  def min(implicit r: FoldLeft[M], ord: Order[A]) =
    foldl1((x: A, y: A) => if (x ≨ y) x else y)

  def longDigits(implicit d: A <:< Digit, t: FoldLeft[M]) =
    foldl[Long](0L, (n, a) => n * 10L + (a: Digit))

  def digits(implicit c: A <:< Char, t: Functor[M]): M[Option[Digit]] =
    ∘((a: A) => (a: Char).digit)

  def sequence[N[_], B](implicit a: A <:< N[B], t: Traverse[M], n: Applicative[N]): N[M[B]] =
    ↦((z: A) => (z: N[B]))

  def traverseDigits(implicit c: A <:< Char, t: Traverse[M]): Option[M[Digit]] = {
    val k = ∘((f: A) => (f: Char)).digits.sequence
    k
  }
}

trait MAs {
  implicit def ma[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val v = a
  }
}
