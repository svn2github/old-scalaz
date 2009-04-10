/*
Java
----
ArrayList
HashMap
HashSet
Hashtable
IdentityHashMap
LinkedHashMap
LinkedHashSet
LinkedList
PriorityQueue
Stack
TreeMap
TreeSet
Vector
WeakHashMap
ArrayBlockingQueue
ConcurrentHashMap
ConcurrentLinkedQueue
CopyOnWriteArrayList
CopyOnWriteArraySet
LinkedBlockingQueue
PriorityBlockingQueue
SynchronousQueue

Scala
-----
Unit
TupleN
FunctionN
PartialFunction
scala.collection.*
scala.collection.mutable.*
scala.collection.immutable.*
List
Stream
Either
Option
Array
*/

trait PartialApply1Of2[T[_, _], A] {
  type Apply[B] = T[A, B]

  type Flip[B] = T[B, A]
}

trait PartialApplyK[T[_[_], _, _], M[_]] {
  type Apply[A, B] = T[M, A, B]
}

sealed trait Identity[+A] {
  val a: A
}

object Identity {
  implicit def id[A](x: A) = new Identity[A] {
    val a = x
  }

  implicit def di[A](i: Identity[A]) = i.a
}

sealed trait Continuation[-A, R] {
  def apply(r: R, a: A): R
}

object Continuation {
  def continuation[A, R](f: (R, A) => R) = new Continuation[A, R] {
    def apply(r: R, a: A) = f(r, a)
  }
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
  def apply(a: A): M[B]
}

object Kleisli {
  sealed trait KleisliApply[M[_]] {
    def apply[A, B](f: A => M[B]): Kleisli[M, A, B]
  }

  def kleisli[M[_]] = new KleisliApply[M] {
    def apply[A, B](f: A => M[B]) = new Kleisli[M, A, B] {
      def apply(a: A) = f(a)
    }
  }
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

  sealed trait TraverseApply[F[_]] {
    def apply[A, B](f: A => F[B], ta: T[A])(implicit a: Applicative[F]): F[T[B]]
  }

  def traverse[F[_]] = new TraverseApply[F] {
    def apply[A, B](f: A => F[B], ta: T[A])(implicit a: Applicative[F]) = trav[F, A, B](f, ta)
  }
}

trait Arrow[A[_, _]] {
  def arrow[B, C](f: B => C): A[B, C]

  def compose[B, C, D](a1: A[B, C], a2: A[C, D]): A[B, D]

  def first[B, C, D](a: A[B, C]): A[(B, D), (C, D)]

  def second[B, C, D](a: A[B, C]): A[(D, B), (D, C)]
}

object Arrow {
  val Function1Arrow = new Arrow[Function1] {
    def arrow[B, C](f: B => C) = f

    def compose[B, C, D](a1: B => C, a2: C => D) =
      a2 compose a1

    def first[B, C, D](a: B => C) =
      (bd: (B, D)) => (a(bd._1), bd._2)

    def second[B, C, D](a: B => C) =
      (db: (D, B)) => (db._1, a(db._2))
  }

  def KleisliArrow[M[+_]](implicit m: Monad[M]) =
        new Arrow[PartialApplyK[Kleisli, M]#Apply] {
    import Kleisli.kleisli

    def arrow[B, C](f: B => C) =
      kleisli[M]((b: B) => m.pure.pure(f(b)))

    def compose[B, C, D](a1: Kleisli[M, B, C], a2: Kleisli[M, C, D]) =
      kleisli[M]((b: B) => m.bind.bind(a1(b), (c: C) => a2(c)))

    def first[B, C, D](a: Kleisli[M, B, C]) =
      kleisli[M].apply[(B, D), (C, D)] { case (b, d) => m.functor.fmap(a(b), (c: C) => (c, d)) }

    def second[B, C, D](a: Kleisli[M, B, C]) =
      kleisli[M].apply[(D, B), (D, C)]{ case (d, b) => m.functor.fmap(a(b), (c: C) => (d, c)) }
  }
}

