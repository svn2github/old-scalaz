sealed trait Identity[+A] {
  val a: A
}

object Identity {
  implicit def id[A](x: A) = new Identity[A] {
    val a = x
  }

  implicit def di[A](i: Identity[A]) = i.a
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
  implicit def pure[A](a: A): P[A]
}

object Pure {
  implicit val IdentityPure = new Pure[Identity] {
    def pure[A](a: A) = Identity.id(a)
  }
}

sealed trait Pointed[P[_]] {
  val functor: Functor[P]
  val pure: Pure[P]
}

object Pointed {
  def pointed[P[_]](implicit f: Functor[P], p: Pure[P]) = new Pointed[P] {
    val functor = f
    val pure = p
  }

  implicit val IdentityPointed = pointed[Identity]
}

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

object Apply {
  implicit val IdentityApply: Apply[Identity] = new Apply[Identity] {
    def apply[A, B](f: Identity[A => B], a: Identity[A]): Identity[B] = Identity.id(f(a))
  }
}

sealed trait Applicative[Z[_]] {
  implicit val pure: Pure[Z]
  implicit val apply: Apply[Z]

  implicit val functor: Functor[Z] = new Functor[Z] {
    def fmap[A, B](fa: Z[A], f: A => B) = apply(pure.pure(f), fa)
  }

  implicit val pointed = Pointed.pointed[Z]
}

object Applicative {
  def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]) = new Applicative[Z] {
    val pure = p
    val apply = a
  }
}

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  implicit def IdentityBind = new Bind[Identity] {
    def bind[A, B](a: Identity[A], f: A => Identity[B]) = f(a)
  }
}

sealed trait Monad[M[_]] {
  implicit val pure: Pure[M]
  implicit val bind: Bind[M]

  implicit val functor = new Functor[M] {
    def fmap[A, B](fa: M[A], f: A => B) = bind.bind(fa, (a: A) => pure.pure(f(a)))
  }

  implicit val pointed = Pointed.pointed[M]

  implicit val apply = new Apply[M] {
    def apply[A, B](f: M[A => B], a: M[A]): M[B] = bind.bind(f, (k: A => B) => functor.fmap(a, k(_: A)))
  }
}

trait Empty[E[_]] {
  def empty[A]: E[A]
}

trait Plus[P[_]] {
  def plus[A](a1: P[A], a2: P[A]): P[A]
}

sealed trait Semigroup[S] {
  def append(s1: S, s2: S): S
}

object Semigroup {
  def semigroup[S](f: (S, S) => S) = new Semigroup[S] {
    def append(s1: S, s2: S) = f(s1, s2)
  }
}

sealed trait Zero[Z] {
  val zero: Z
}

object Zero {
  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }
}

sealed trait Monoid[M] {
  implicit val semigroup: Semigroup[M]
  val zero: Zero[M]
}

object Monoid {
  def monoid[M](implicit s: Semigroup[M], z: Zero[M]) = new Monoid[M] {
    val semigroup = s
    val zero = z
  }
}

trait Kleisli[M[_], -A, B] {
  def kleisli(a: A): M[B]
}

trait Cofunctor[F[_]] {
  def comap[A, B](fa: F[A], f: B => A): F[B]
}

trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](k: F[A, B], f: A => C, g: B => D): F[C, D]
}

trait Each[E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
}

trait FoldLeft[F[_]] {
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

trait FoldRight[F[_]] {
  def foldRight[A, B](t: F[A], b: B, f: (A, B) => B): B
}

trait Paramorphism[P[_]] {
  def para[A, B](fa: P[A], b: B, f: (A, P[A], B) => B): B
}

trait Traverse[T[_]] {
  def trav[F[_], A, B](f: A => F[B], ta: T[A])(implicit a: Applicative[F]): F[T[B]]

  trait TraverseApply[F[_]] {
    def apply[A, B](f: A => F[B], ta: T[A])(implicit a: Applicative[F]): F[T[B]]
  }

  def traverse[F[_]] = new TraverseApply[F] {
    def apply[A, B](f: A => F[B], ta: T[A])(implicit a: Applicative[F]) = trav[F, A, B](f, ta)
  }
}


// todo Traverse, Arrow
