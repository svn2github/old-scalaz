package scalaz

trait Functor[F[_]] {
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

object Functor {
  implicit val Function0Functor = new Functor[Function0] {
    def fmap[A, B](r: Function0[A], f: A => B) = new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit val ListFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](r: List[A], f: A => B) = r map f
  }
}

trait Pure[+P[_]] {
  def pure[A](a: => A): P[A]
}

object Pure {
  implicit val Function0Pure = new Pure[Function0] {
    def pure[A](a: => A) = new Function0[A] {
      def apply = a
    }
  }

  implicit val ListPure = new Pure[List] {
    def pure[A](a: => A) = List(a)
  }
}

trait Pointed[P[_]] extends Functor[P] with Pure[P]

object Pointed {
  implicit def pointed[P[_]](implicit t: Functor[P], p: Pure[P]) = new Pointed[P] {
    def fmap[A, B](a: P[A], f: A => B) = t.fmap(a, f)
    def pure[A](a: => A): P[A] = p.pure(a)
  }
}

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

object Apply {
  import Scalaz._

  implicit val Function0Apply: Apply[Function0] = FunctorBindApply

  implicit val ListApply: Apply[List] = FunctorBindApply
}

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  implicit val Function0Bind: Bind[Function0] = new Bind[Function0] {
    def bind[A, B](r: Function0[A], f: A => Function0[B]) = f(r.apply)
  }

  implicit val ListBind: Bind[List] = new Bind[List] {
    def bind[A, B](r: List[A], f: A => List[B]) = r flatMap f
  }
}

trait Applicative[Z[_]] extends Pointed[Z] with Apply[Z] {
  override def fmap[A, B](fa: Z[A], f: A => B) = this(pure(f), fa)
}

object Applicative {
  implicit def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]) = new Applicative[Z] {
    def pure[A](a: => A) = p.pure(a)
    def apply[A, B](f: Z[A => B], x: Z[A]) = a(f, x)
  }
}

trait Monad[M[_]] extends Applicative[M] with Bind[M] with Pointed[M] {
  override def fmap[A, B](fa: M[A], f: A => B) = bind(fa, (a: A) => pure(f(a)))
  override def apply[A, B](f: M[A => B], a: M[A]): M[B] = bind(f, (k: A => B) => fmap(a, k(_: A)))
}

object Monad {
  implicit def monad[M[_]](implicit b: Bind[M], p: Pure[M]) = new Monad[M] {
    def pure[A](a: => A) = p.pure(a)
    def bind[A, B](a: M[A], f: A => M[B]) = b.bind(a, f)
  }
}

object Scalaz {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }
}

sealed trait MA[M[_], A] {
  val v: M[A]

  def ∘[B](f: A => B)(implicit t: Functor[M]) = t.fmap(v, f)

  def map[B](f: A => B)(implicit t: Functor[M]) = ∘(f)

  def >|[B](f: => B)(implicit t: Functor[M]) = ∘(_ => f)
}

object MA {
  implicit def ma[M[_], A](a: M[A]) = new MA[M, A] {
    val v = a
  }
}

////

object Example {
  import MA._

  def main(args: Array[String]) {
    println(List(1, 2, 3, 4, 5) ∘ (1+))
    println(List(1, 2, 3, 4) >| "boo")
  }
}