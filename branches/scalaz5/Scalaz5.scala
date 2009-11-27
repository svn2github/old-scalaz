package scalaz

trait PartialApply1Of2[T[_, _], A] {
  type Apply[B] = T[A, B]

  type Flip[B] = T[B, A]
}


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

trait Cofunctor[F[_]] {
  def comap[A, B](r: F[A], f: B => A): F[B]
}

object Cofunctor {
  implicit def Function1Cofunctor[X]: Cofunctor[PartialApply1Of2[Function1, X]#Flip] = new Cofunctor[PartialApply1Of2[Function1, X]#Flip] {
    def comap[A, B](r: A => X, f: B => A) = r compose f
  }
}

trait Plus[P[_]] {
  def plus[A](a1: P[A], a2: => P[A]): P[A]
}

object Plus {
  implicit val ListPlus = new Plus[List] {
    def plus[A](a1: List[A], a2: => List[A]) = a1 ::: a2
  }
}

object Each {
  implicit val IterableEach = new Each[Iterable] {
    def each[A](e: Iterable[A], f: A => Unit) = e foreach f
  }
}

trait Each[-E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
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

  def ∗[B](f: A => M[B])(implicit b: Bind[M]) = b.bind(v, f)

  def ∗|[B](f: => M[B])(implicit b: Bind[M]) = ∗(_ => f)

  def flatMap[B](f: A => M[B])(implicit b: Bind[M]) = ∗(f)

  def ⟴(z: => M[A])(implicit p: Plus[M]) = p.plus(v, z)

  def ➝:(a: A)(implicit p: Plus[M], q: Pure[M]) = p.plus(q.pure(a), v)

  def ➡(f: A => Unit)(implicit e: Each[M]) = e.each(v, f)

  def foreach(f: A => Unit)(implicit e: Each[M]) = ➡(f)
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
    // Functor map
    println(List(1, 2, 3, 4, 5) ∘ (1+))

    // Functor map (anonymous)
    println(List(1, 2, 3, 4) >| "boo")

    // Contravariant functor map
    {
      val f = ma[PartialApply1Of2[Function1, Int]#Flip, Int](3+) // todo This is icky. Posted to mailing list.
      println(List(1, 2, 3, 4, 5) ∘ (f ∙ ((_: Int) / 2)))
    }

    // Applicative functor apply
    println(List(40, 50, 60) ⊛ (List(1, 2, 3) ∘ ((_: Int) * (_: Int)).curry))

    // Applicative functor lift
    println(List(1, 2, 3) <⊛> (List(40, 50, 60), (_: Int) * (_: Int)))

    // Applicative functor lift to pair
    println(List(1, 2, 3) <<⊛>> List(40, 50, 60))

    // Applicative functor lift (anonymous right)
    println(List(1, 2, 3) ⊛> List(40, 50, 60))

    // Applicative functor lift (anonymous left)
    println(List(1, 2, 3) <⊛ List(40, 50, 60))

    // Monad bind
    println(List(1, 2, 3) ∗ (n => List(7, n)))

    // Plus
    println(List(1, 2, 3) ⟴ List(4, 5, 6))

    // Pure/Plus
    println(1 ➝: 2 ➝: 3 ➝: List(4, 5, 6))

    // Each
    List(1, 2, 3) ➡ print; println
  }
}
