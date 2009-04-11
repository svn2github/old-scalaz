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

Other
-----
Identity
Continuation
NonEmptyList
Validation
*/

sealed trait Function1W[-T, +R] {
  val k: T => R

  def on[X](f: (R, R) => X, t1: T, t2: T) = f(k(t1), k(t2))

  def arrow[A[-_, +_]](implicit a: Arrow[A]) = a arrow k
}

object Function1W {
  def function1[T, R](f: T => R) = new Function1W[T, R] {
    val k = f
  }
}

trait PartialApply1Of2[T[_, _], A] {
  type Apply[B] = T[A, B]

  type Flip[B] = T[B, A]
}

trait PartialApplyK[T[_[_], _, _], M[_]] {
  type Apply[A, B] = T[M, A, B]
}

sealed trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

object Equal {
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }
}

sealed trait Ordering {
  val toInt: Int
}
final case object LT extends Ordering {
  val toInt = -1
}
final case object EQ extends Ordering {
  val toInt = 0
}
final case object GT extends Ordering {
  val toInt = 1
}

sealed trait Order[-A] {
  def order(a1: A, a2: A): Ordering
}

object Order {
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }
}

trait Show[-A] {
  def show(a: A): List[Char]
}

object Show {
  def show[A](f: A => List[Char]) = new Show[A] {
    def show(a: A) = f(a)
  }
}

sealed trait Identity[A] {
  val value: A

  def pure[P[_]](implicit p: Pure[P]) = p pure value

  def |+|(a: A)(implicit s: Semigroup[A]) = s append (value, a)

  def ===(a: A)(implicit e: Equal[A]) = e equal (value, a)

  def /=(a: A)(implicit e: Equal[A]) = !(===(a))

  override def toString = value.toString

  override def hashCode = value.hashCode

  override def equals(o: Any) = o.isInstanceOf[Identity[_]] && value == o.asInstanceOf[Identity[_]].value
}

object Identity {
  implicit def id[A](x: A) = new Identity[A] {
    val value = x
  }

  val u = id(())
}

sealed trait Continuation[R, +A] {
  def apply(f: A => R): R

  import Continuation._

  def using[AA >: A, B](f: (B => R) => AA => R) = {
    continuation[R, B](f andThen apply)
  }
}

object Continuation {
  def continuation[R, A](f: (A => R) => R) = new Continuation[R, A] {
    def apply(k: A => R) = f(k)
  }

  trait ContinuationConstant[A] {
    def apply[R](r: => R): Continuation[R, A]
  }

  def constant[A] = new ContinuationConstant[A] {
    def apply[R](r: => R) = continuation[R, A](_ => r)
  }
}

trait Functor[F[_]] {
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

object Functor {
  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](r: Identity[A], f: A => B) = Identity.id(f(r.value))
  }

  implicit def ContinuationFunctor[R] = new Functor[PartialApply1Of2[Continuation, R]#Apply] {
    def fmap[A, B](r: Continuation[R, A], f: A => B) = Continuation.continuation[R, B](k => r(k compose f))
  }

  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](r: Option[A], f: A => B) = r map f
  }
}

trait Pure[P[_]] {
  implicit def pure[A](a: A): P[A]
}

object Pure {
  implicit val IdentityPure = new Pure[Identity] {
    def pure[A](a: A) = Identity.id(a)
  }

  implicit def ContinuationPure[R] = new Pure[PartialApply1Of2[Continuation, R]#Apply] {
    def pure[A](a: A) = Continuation.continuation[R, A](_(a))
  }

  implicit def OptionPure = new Pure[Option] {
    def pure[A](a: A) = Some(a)
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

  implicit def ContinuationPointed[R] = pointed[PartialApply1Of2[Continuation, R]#Apply]

  implicit val OptionPointed = pointed[Option]
}

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

object Apply {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }

  implicit val IdentityApply: Apply[Identity] = FunctorBindApply[Identity]

  implicit def ContinuationApply[R] = FunctorBindApply[PartialApply1Of2[Continuation, R]#Apply]

  implicit def OptionApply = FunctorBindApply[Option]
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

  implicit val IdentityApplicative = applicative[Identity]

  implicit def ContinuationApplicative[R] = applicative[PartialApply1Of2[Continuation, R]#Apply]

  implicit def OptionApplicative = applicative[Option]
}

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  implicit val IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](a: Identity[A], f: A => Identity[B]) = f(a.value)
  }

  implicit def ContinuationBind[R]: Bind[PartialApply1Of2[Continuation, R]#Apply] = new Bind[PartialApply1Of2[Continuation, R]#Apply] {
    def bind[A, B](a: Continuation[R, A], f: A => Continuation[R, B]) = Continuation.continuation[R, B](c => a(p => f(p)(c)))
  }

  implicit def OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](a: Option[A], f: A => Option[B]) = a flatMap f
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

object Monad {
  def monad[M[_]](implicit b: Bind[M], p: Pure[M]) = new Monad[M] {
    val pure = p
    val bind = b
  }

  implicit val IdentityMonad = monad[Identity]

  implicit def ContinuationMonad[R] = monad[PartialApply1Of2[Continuation, R]#Apply]

  implicit def OptionMonad = monad[Option]
}

trait Empty[E[_]] {
  def empty[A]: E[A]
}

object Empty {
  implicit val OptionEmpty = new Empty[Option] {
    def empty[A] = None
  }
}

trait Plus[P[_]] {
  def plus[A](a1: P[A], a2: P[A]): P[A]
}

object Plus {
  implicit val OptionPlus = new Plus[Option] {
    def plus[A](a1: Option[A], a2: Option[A]) = a1 orElse a2
  }
}

sealed trait Semigroup[S] {
  def append(s1: S, s2: S): S
}

object Semigroup {
  def semigroup[S](f: (S, S) => S) = new Semigroup[S] {
    def append(s1: S, s2: S) = f(s1, s2)
  }

  implicit val StringSemigroup = semigroup[String](_ + _)
}

sealed trait Zero[Z] {
  val zero: Z
}

object Zero {
  def z[Z](implicit x: Zero[Z]) = x.zero

  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  implicit val StringZero = zero("")
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

  implicit val StringMonoid = monoid[String]
}

trait Kleisli[M[_], -A, B] {
  def apply(a: A): M[B]

  def |=>(a: A) = apply(a)

  import Kleisli.kleisli

  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]) = kleisli[M]((a: A) => b.bind(this(a), k(_: B)))

  def >=>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = >=>(kleisli[M](k))

  def compose[N[_]](f: M[B] => N[B]) = kleisli[N]((a: A) => f(this(a)))

  trait TraverseK[F[_]] {
    def apply[AA <: A](f: F[AA])(implicit a: Applicative[M], t: Traverse[F]): M[F[B]]
  }

  def traverse[F[_]] = new TraverseK[F] {
    def apply[AA <: A](f: F[AA])(implicit a: Applicative[M], t: Traverse[F]): M[F[B]] = t.traverse[M, AA, B](Kleisli.this(_), f)
  }
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
  def comap[A, B](r: F[A], f: B => A): F[B]
}

object Cofunctor {
  implicit def Function1Cofunctor[X]: Cofunctor[PartialApply1Of2[Function1, X]#Flip] = new Cofunctor[PartialApply1Of2[Function1, X]#Flip] {
    def comap[A, B](r: A => X, f: B => A) = r compose f
  }
}

trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](k: F[A, B], f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  implicit def Tuple2Bifunctor = new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](k: (A, B), f: A => C, g: B => D) =
      (f(k._1), g(k._2))
  }

  implicit def EitherBifunctor = new Bifunctor[Either] {
    def bimap[A, B, C, D](k: Either[A, B], f: A => C, g: B => D) =
      k match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }
}

trait Each[E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
}

object Each {
  implicit val IdentityEach = new Each[Identity] {
    def each[A](e: Identity[A], f: A => Unit) = f(e.value)
  }
}

trait FoldLeft[F[_]] {
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val IdentityFoldLeft = new FoldLeft[Identity] {
    def foldLeft[B, A](t: Identity[A], b: B, f: (B, A) => B) = f(b, t.value)
  }
}

trait FoldRight[F[_]] {
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
}

object FoldRight {
  implicit val IdentityFoldRight = new FoldRight[Identity] {
    def foldRight[A, B](t: Identity[A], b: B, f: (A, => B) => B) = f(t.value, b)
  }
}

trait Paramorphism[P[_]] {
  def para[A, B](fa: P[A], b: B, f: (=> A, => P[A], B) => B): B
}

object Paramorphism {
  implicit val OptionParamorphism = new Paramorphism[Option] {
    def para[A, B](as: Option[A], b: B, f: ((=> A, => Option[A], B) => B)): B = as match {
      case None => b
      case Some(a) => f(a, None, b)
    }
  }
}

trait Traverse[T[_]] {
  def traverse[F[_], A, B](f: A => F[B], t: T[A])(implicit a: Applicative[F]): F[T[B]]
}

object Traverse {
  implicit val IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_], A, B](f: A => F[B], t: Identity[A])(implicit a: Applicative[F]) = a.functor.fmap(f(t.value), Identity.id(_: B))
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

///////////////////////////////////////////////////////////////////////////////



trait PartialWrapMA[M[_], V[_[_], _]] {
  def apply[A](a: M[A]): V[M, A]
}

trait PartialWrapMMA[M[_], V[_[_], _]] {
  def apply[A](a: M[M[A]]): V[M, A]
}

trait PartialWrapMAB[M[_, _], V[_[_, _], _, _]] {
  def apply[A, B](a: M[A, B]): V[M, A, B]
}

sealed trait MA[M[_], A] {
  val v: M[A]

  def map[B](f: A => B)(implicit t: Functor[M]) = t.fmap(v, f)

  def |>[B](f: A => B)(implicit t: Functor[M]) = map(f)

  def <|:[B](f: A => B)(implicit t: Functor[M]) = map(f)

  def |>-[B](f: => B)(implicit t: Functor[M]) = map(_ => f)

  def -<|:[B](f: => B)(implicit t: Functor[M]) = |>-(f)

  def <*>[B](f: M[A => B])(implicit a: Apply[M]) = a(f, v)

  def <*>:[B](f: M[A => B])(implicit a: Apply[M]) = <*>(f)

  def *>[B](k: M[B])(implicit f: Functor[M], a: Apply[M]) = a(f.fmap(v, (_: A) => (b: B) => b), k)

  def <*[B](k: M[B])(implicit f: Functor[M], a: Apply[M]) = a(f.fmap(v, (a: A) => (_: B) => a), k)

  def <**>[B](k: M[B])(implicit f: Functor[M], a: Apply[M]) = a(f.fmap(v, (a: A) => (b: B) => (a, b)), k)

  def liftA[B, C](b: M[B], z: A => B => C)(implicit f: Functor[M], a: Apply[M]) = a(f.fmap(v, z), b)

  def liftA[B, C, D](b: M[B], c: M[C], z: A => B => C => D)(implicit f: Functor[M], a: Apply[M]) =
    a(a(f.fmap(v, z), b), c)

  def liftA[B, C, D, E](b: M[B], c: M[C], d: M[D], z: A => B => C => D => E)(implicit f: Functor[M], a: Apply[M]) =
    a(a(a(f.fmap(v, z), b), c), d)

  def liftA[B, C, D, E, F](b: M[B], c: M[C], d: M[D], e: M[E], z: A => B => C => D =>
 E => F)(implicit f: Functor[M], a: Apply[M]) =
    a(a(a(a(f.fmap(v, z), b), c), d), e)

  def <<*>>[B](b: M[B])(implicit f: Functor[M], a: Apply[M]) = liftA(b, a => (b: B) => (a, b))

  def <<*>>[B, C](b: M[B], c: M[C])(implicit f: Functor[M], a: Apply[M]) = liftA(b, c, a => (b: B) => (c: C) => (a, b, c))

  def <<*>>[B, C, D](b: M[B], c: M[C], d: M[D])(implicit f: Functor[M], a: Apply[M]) = liftA(b, c, d, a => (b: B) => (c: C)
 => (d: D) => (a, b, c, d))

  def <<*>>[B, C, D, E](b: M[B], c: M[C], d: M[D], e: M[E])(implicit f: Functor[M], a: Apply[M]) = liftA(b, c, d, e, a =>
(b: B) => (c: C) => (d: D) => (e: E) => (a, b, c, d, e))

  def >>=[B](f: A => M[B])(implicit b: Bind[M]) = b.bind(v, f)

  def flatMap[B](f: A => M[B])(implicit b: Bind[M]) = >>=(f)

  def >->[B](f: => M[B])(implicit b: Bind[M]) = >>=(_ => f)

  def <+>(z: M[A])(implicit p: Plus[M]) = p.plus(v, z)

  def <|[B](f: B => A)(implicit t: Cofunctor[M]) = t.comap(v, f)

  def <|:[B](f: B => A)(implicit t: Cofunctor[M]) = <|(f)

  def -<|[B](f: => A)(implicit t: Cofunctor[M]) = <|((_: B) => f)

  def |>-:[B](f: => A)(implicit t: Cofunctor[M]) = -<|(f)

  def foreach(f: A => Unit)(implicit e: Each[M]) = e.each(v, f)

  def ->>(f: A => Unit)(implicit e: Each[M]) = foreach(f)

  def foldl[B](b: B, f: (B, A) => B)(implicit r: FoldLeft[M]) = r.foldLeft[B, A](v, b, f)

  def foldl1(f: (A, A) => A)(implicit r: FoldLeft[M]) = foldl[Option[A]](None, (a1, a2) => Some(a1 match {
    case None => a2
    case Some(x) => f(a2, x)
  })) getOrElse (error("foldl1 on empty"))

  def listl(implicit r: FoldLeft[M]) = {
    val b = new scala.collection.mutable.ListBuffer[A]
    foldl[scala.Unit]((), (x, a) => b += a)
    b.toList
  }

  def suml(implicit r: FoldLeft[M], m: Monoid[A]) = foldl[A](m.zero.zero, m.semigroup append (_, _))

  def items(implicit r: FoldLeft[M]) = foldl[Int](0, (b, _) => b + 1)

  def max(implicit r: FoldLeft[M], ord: Order[A]) =
    foldl1((x: A, y: A) => if(ord.order(x, y) == GT) x else y)

  def min(implicit r: FoldLeft[M], ord: Order[A]) =
    foldl1((x: A, y: A) => if(ord.order(x, y) == LT) x else y)

  def foldr[B](b: B, f: (A, => B) => B)(implicit r: FoldRight[M]) = r.foldRight(v, b, f)

  def foldr1(f: (A, => A) => A)(implicit r: FoldRight[M]) = foldr[Option[A]](None, (a1, a2) => Some(a2 match {
    case None => a1
    case Some(x) => f(a1, x)
  })) getOrElse (error("foldr1 on empty"))

  def suml(implicit r: FoldRight[M], m: Monoid[A]) = foldr[A](m.zero.zero, m.semigroup append (_, _))

  def listr(implicit r: FoldRight[M]) = foldr[List[A]](Nil, _ :: _)

  def stream(implicit r: FoldRight[M]) = foldr[Stream[A]](Stream.empty, Stream.cons(_, _))

  def !(n: Int)(implicit r: FoldRight[M]) = stream(r)(n)

  def any(p: A => Boolean)(implicit r: FoldRight[M]) = foldr[Boolean](false, p(_) || _)

  def all(p: A => Boolean)(implicit r: FoldRight[M]) = foldr[Boolean](true, p(_) && _)

  def nil(implicit r: FoldRight[M]) = all(_ => false)

  def splitWith(p: A => Boolean)(implicit r: FoldRight[M]) = foldr[(List[List[A]], Option[Boolean])]((Nil, None), (
a, b) => {
      val pa = p(a)
      (b match {
        case (_, None) => List(List(a))
        case (x, Some(q)) => if(pa == q) (a :: x.head) :: x.tail else List(a) :: x
      }, Some(pa))
    })._1

  def selectSplit(p: A => Boolean)(implicit r: FoldRight[M]) = foldr[(List[List[A]], Boolean)]((Nil, false), (a, xb
) => xb match {
      case (x, b) => {
        val pa = p(a)
        (if(pa) if(b) (a :: x.head) :: x.tail else List(a) :: x else x, pa)
      }
    })._1

  def para[B](b: B, f: (=> A, => M[A], B) => B)(implicit p: Paramorphism[M]) = p.para(v, b, f)

  trait TraverseM[F[_]] {
    def apply[B](f: A => F[B])(implicit a: Applicative[F]): F[M[B]]
  }

  def traverse[F[_]](implicit t: Traverse[M]) = new TraverseM[F] {
    def apply[B](f: A => F[B])(implicit a: Applicative[F]) = t.traverse[F, A, B](f, v)
  }

  def ==>>[B](f: A => B)(implicit t: Traverse[M], m: Monoid[B]): B = {
    case class Acc[B, A](acc: B)

    implicit val AccApply = new Apply[PartialApply1Of2[Acc, B]#Apply] {
      def apply[A, X](f: Acc[B, A => X], fa: Acc[B, A]) = Acc[B, X](m.semigroup append (f.acc, fa.acc))
    }

    implicit val AccPure = new Pure[PartialApply1Of2[Acc, B]#Apply] {
      def pure[A](a: A) = Acc[B, A](m.zero.zero)
    }

    implicit val AccApplicative = Applicative.applicative[PartialApply1Of2[Acc, B]#Apply]

    traverse[PartialApply1Of2[Acc, B]#Apply](t)(a => Acc[B, B](f(a))).acc
  }

  def =>>(implicit t: Traverse[M], m: Monoid[A]) = ==>>(identity[A])
}

object MA {
  def ma[M[_]] = new PartialWrapMA[M, MA] {
    def apply[A](a: M[A]) = new MA[M, A] {
      val v = a
    }
  }

  implicit def IdentityMA[A](a: Identity[A]) = ma[Identity](a)

  implicit def ContinuationMA[R, A](a: Continuation[R, A]) = ma[PartialApply1Of2[Continuation, R]#Apply](a)
}

sealed trait MAB[M[_, _], A, B] {
  val v: M[A, B]

  def :->[D](g: B => D)(implicit b: Bifunctor[M]) = b.bimap(v, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]) = b.bimap(v, f, identity[B])

  def >>>[C](k: M[B, C])(implicit a: Arrow[M]) = a compose (v, k)

  def fst[C](implicit a: Arrow[M]): M[(A, C), (B, C)] = a first v

  def snd[C](implicit a: Arrow[M]): M[(C, A), (C, B)] = a second v

  def ***[C, D](k: M[C, D])(implicit a: Arrow[M]) = a.compose(fst[C], a.second[C, D, B](k))

  def &&&[C](k: M[A, C])(implicit a: Arrow[M]): M[A, (B, C)] = a.compose(a.arrow(a => (a, a)), ***(k))
}

object MAB {
  def mab[M[_, _]] = new PartialWrapMAB[M, MAB] {
    def apply[A, B](a: M[A, B]) = new MAB[M, A, B] {
      val v = a
    }
  }
}

sealed trait MMA[M[_], A] {
  val v: M[M[A]]

  def join(implicit b: Bind[M]) = b.bind(v, (x: M[A]) => x)
}

object MMA {
  def mma[M[_]] = new PartialWrapMMA[M, MMA] {
    def apply[A](m: M[M[A]]) = new MMA[M, A] {
      val v = m
    }
  }

  implicit def IdentityZZ[A](a: Identity[Identity[A]]) = mma[Identity](a)
}

///////////////////////////////////////////////////////////////////////////////

object Demo {
  import Identity._
  import MA._
  import MMA._

  def main(args: Array[String]) {
    val j: Identity[Identity[Int]] = (93: Identity[Int])
    val k: Identity[Int] = 72
    val f: Identity[Int => Int] = ((_: Int) + 1)
    val g: Int => Identity[String] = ((n: Int) => Identity.id(n.toString.reverse))

    // Pure
    println(7.pure[Identity])

    // Functor
    println(k |> ((_: Int) + 1))
    println(for(z <- k) yield z + 1)

    // Apply
    println(k <*> f)

    // Bind
    println(k >>= g)
    println(for(z <- k; n <- g(z)) yield n)

    // ZZ
    println(j join)
  }
}