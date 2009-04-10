sealed trait Identity[+A] {
  val a: A
}

object Identity {
  implicit def id[A](x: A) = new Identity[A] {
    val a = x
  }

  implicit def Di[A](i: Identity[A]) = i.a
}

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A], f: A => B): F[B]
}

object Functor {
  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](fa: Identity[A], f: A => B) = Identity.id(f(fa))
  }
}

trait Pure[P[_]] {
  def pure[A](a: A): P[A]
}

object Pure {
  implicit val IdentityPure = new Pure[Identity] {
    def pure[A](a: A) = Identity.id(a)
  }
}
