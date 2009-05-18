package scalaz

sealed trait Identity[A] {
  val value: A

  import S._

  def |+|(a: => A)(implicit s: Semigroup[A]) = s append (value, a)

  def ===(a: A)(implicit e: Equal[A]) = e equal (value, a)

  def /=(a: A)(implicit e: Equal[A]) = !(===(a))

  def ?:?(a: A)(implicit o: Order[A]) = o order (value, a)

  def show(implicit s: Show[A]) = s.show(value)

  def shows(implicit s: Show[A]) = s.shows(value)

  def print(implicit s: Show[A]) = Console.print(shows)

  def println(implicit s: Show[A]) = Console.println(shows)

  def text(implicit s: Show[A]) = xml.Text(s shows value)

  def constantState[S, A](s: S) = State.state((_: S) => (s, value))

  def state[S] = State.state((_: S, value))

  def dual = Dual.dual(value)

  sealed trait Unfold[M[_]] {
    def apply[B](f: A => Option[(B, A)])(implicit p: Pure[M], m: Monoid[M[B]]): M[B]
  }

  def unfold[M[_]]: Unfold[M] = new Unfold[M] {
    def apply[B](f: A => Option[(B, A)])(implicit p: Pure[M], m: Monoid[M[B]]) = f(value) match {
      case None => m.zero
      case Some((b, a)) => b.pure[M] |+| a.unfold[M](f)
    }
  }

  def replicate[M[_]](n: Int)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    if (n <= 0) m.zero
    else value.pure[M] |+| replicate[M](n - 1)

  def repeat[M[_]](implicit p: Pure[M], m: Monoid[M[A]]): M[A] = value.pure[M] |+| repeat[M]

  def iterate[M[_]](f: A => A)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    value.pure[M] |+| f(value).iterate[M](f)

  def zipper = S.zipper(Stream.empty, value, Stream.empty)

  def <===>(a: A)(implicit m: MetricSpace[A]) = m distance (value, a)

  def success[X]: Validation[X, A] = Success(value)

  def fail[X]: Validation[A, X] = Failure(value)

  import StreamW._
  def unfoldTree[B](f: A => (B, () => Stream[A])): Tree[B] = f(value) match {
    case (a, bs) => Tree.node(a, bs.apply.unfoldForest(f))
  }

  def unfoldTreeM[B, M[_]](f: A => M[(B, Stream[A])])(implicit m: Monad[M]): M[Tree[B]] = {
    m.bind(f(value), (abs: (B, Stream[A])) =>
        m.bind(abs._2.unfoldForestM[B, M](f), (ts: Stream[Tree[B]]) =>
            m.pure(Tree.node(abs._1, ts))))
  }

  import test._

  def check(minSuccessful: Int, maxDiscarded: Int, minSize: Int, maxSize: Int)(implicit t: Testable[A], r: Rand): Result = {
    var s = 0
    var d = 0
    var sz = minSize.toFloat
    var res: Result = null
    var l = true
    val p = t test value

    while(l) {
      val size = if(s == 0 && d == 0) minSize.toFloat else sz + (maxSize - sz) / (minSuccessful - s)

      try {
        p.gen(size.round) fold
                // proven
                (as => { res = Result.result(Status.proven(as), s + 1, d); l = false },
                // unfalsified
                if(s + 1 >= minSuccessful) { Result.result(Status.unfalsified, s + 1, d); l = false } else { sz = size; s = s + 1 },
                // falsified
                as => { res = Result.result(Status.falsified(as), s, d); l = false },
                // undecided
                if(d + 1 >= maxDiscarded) { res = Result.result(Status.undecided, s, d + 1); l = false } else { sz = size; d = d + 1 },
                // exception
                (as, t) => { Result.result(Status.exception(as, t), s, d); l = false })
      } catch {
        case e => { Result.result(e, s, d); l = false }
      }

    }

    res
  }

  override def toString = value.toString

  override def hashCode = value.hashCode

  override def equals(o: Any) = o.isInstanceOf[Identity[_]] && value == o.asInstanceOf[Identity[_]].value
}

object Identity {
  implicit def IdentityTo[A](x: A) = new Identity[A] {
    val value = x
  }

  val u = IdentityTo(())
}
