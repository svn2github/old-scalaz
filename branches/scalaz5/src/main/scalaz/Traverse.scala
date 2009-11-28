package scalaz

trait Traverse[T[_]] extends Functor[T] {
  def traverse[F[_], A, B](f: A => F[B], t: T[A])(implicit a: Applicative[F]): F[T[B]]

  import Scalaz._

  override def fmap[A, B](k: T[A], f: A => B) = traverse[Identity, A, B](f(_), k)
}

object Traverse {
  import Scalaz._

  implicit val IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_], A, B](f: A => F[B], t: Identity[A])(implicit a: Applicative[F]) = a.fmap(f(t), (b: B) => b)
  }

  implicit val Function0Traverse: Traverse[Function0] = new Traverse[Function0] {
    def traverse[F[_], A, B](f: A => F[B], t: Function0[A])(implicit a: Applicative[F]) = a.fmap(f(t.apply), (b: B) => () => b)
  }

  implicit val ListTraverse: Traverse[List] = new Traverse[List] {
    def traverse[F[_], A, B](f: A => F[B], as: List[A])(implicit a: Applicative[F]): F[List[B]] =
      as.reverse.foldLeft[F[List[B]]](a.pure(Nil))((ys, x) => a(a.fmap(f(x), (a: B) => (b: List[B]) => a :: b), ys))
  }
}
