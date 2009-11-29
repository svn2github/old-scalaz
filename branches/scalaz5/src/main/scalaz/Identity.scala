package scalaz

sealed trait Identity[A] {
  val value: A

  import Scalaz._

  def η[F[_]](implicit p: Pure[F]) = p pure value

  def σ: Dual[A] = value
  
  def ⊹(a: => A)(implicit s: Semigroup[A]) = s append (value, a)

  def ≟(a: A)(implicit e: Equal[A]) = e equal (value, a)

  def ≠(a: A)(implicit e: Equal[A]) = !(≟(a))

  def ?|?(a: A)(implicit o: Order[A]) = o order (value, a)

  def ≤(a: A)(implicit o: Order[A]) = o.order(value, a) != GT

  def ≥(a: A)(implicit o: Order[A]) = o.order(value, a) != LT

  def ≨(a: A)(implicit o: Order[A]) = o.order(value, a) == LT

  def ≩(a: A)(implicit o: Order[A]) = o.order(value, a) == GT

  def ≮(a: A)(implicit o: Order[A]) = o.order(value, a) != LT

  def ≯(a: A)(implicit o: Order[A]) = o.order(value, a) != GT

  def ≰(a: A)(implicit o: Order[A]) = o.order(value, a) == GT

  def ≱(a: A)(implicit o: Order[A]) = o.order(value, a) == LT

  def show(implicit s: Show[A]) = s.show(value)

  def shows(implicit s: Show[A]) = s.shows(value)

  def print(implicit s: Show[A]) = Console.print(shows)

  def println(implicit s: Show[A]) = Console.println(shows)

  def text(implicit s: Show[A]) = xml.Text(s shows value)

  def <===>(a: A)(implicit m: MetricSpace[A]) = m distance (value, a)

  def constantState[S, A](s: => S) = state((_: S) => (s, value))

  def state[S] = Scalaz.state((_: S, value))

  def unfold[M[_], B](f: A => Option[(B, A)])(implicit p: Pure[M], m: Monoid[M[B]]): M[B] = f(value) match {
    case None => m.zero
    case Some((b, a)) => b.η ⊹ a.unfold(f)
  }

  def replicate[M[_]](n: Int)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    if (n <= 0) ∅
    else value.η ⊹ replicate(n - 1)

  def repeat[M[_]](implicit p: Pure[M], m: Monoid[M[A]]): M[A] = value.η ⊹ repeat

  def iterate[M[_]](f: A => A)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    value.η ⊹ f(value).iterate(f)

  // todo def zipper = Scalaz.zipper(Stream.empty, value, Stream.empty)

  def unfoldTree[B](f: A => (B, () => Stream[A])): Tree[B] = f(value) match {
    case (a, bs) => node(a, bs.apply.unfoldForest(f))
  }

  def unfoldTreeM[B, M[_]](f: A => M[(B, Stream[A])])(implicit m: Monad[M]): M[Tree[B]] = {
    m.bind(f(value), (abs: (B, Stream[A])) =>
        m.bind(abs._2.unfoldForestM[B, M](f), (ts: Stream[Tree[B]]) =>
            m.pure(node(abs._1, ts))))
  }

  def success[X]: Validation[X, A] = Scalaz.success(value)

  def fail[X]: Validation[A, X] = Scalaz.failure(value)

  def dlist = Scalaz.dlist(value :: (_: List[A]))

  def nel: NonEmptyList[A] = Scalaz.nel(value, Nil)

  override def toString = value.toString

  override def hashCode = value.hashCode

  override def equals(o: Any) = o.isInstanceOf[Identity[_]] && value == o.asInstanceOf[Identity[_]].value
}

trait Identitys {
  implicit def IdentityTo[A](x: A): Identity[A] = new Identity[A] {
    val value = x
  }

  implicit def IdentityFrom[A](x: Identity[A]): A = x.value

  val unital = IdentityTo(())
}
