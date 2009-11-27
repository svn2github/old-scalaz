package scalaz

trait PartialApply1Of2[T[_, _], A] {
  type Apply[B] = T[A, B]

  type Flip[B] = T[B, A]
}


trait Functor[F[_]] {
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

object Functor {
  import Scalaz._

  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](r: Identity[A], f: A => B) = f(r.value)
  }

  implicit val Function0Functor = new Functor[Function0] {
    def fmap[A, B](r: Function0[A], f: A => B) = new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit val ListFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](r: List[A], f: A => B) = r map f
  }

  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](r: Option[A], f: A => B) = r map f
  }
}

trait Pure[+P[_]] {
  def pure[A](a: => A): P[A]
}

object Pure {
  import Scalaz._

  implicit val IdentityPure: Pure[Identity] = new Pure[Identity] {
    def pure[A](a: => A) = a
  }

  implicit val Function0Pure = new Pure[Function0] {
    def pure[A](a: => A) = new Function0[A] {
      def apply = a
    }
  }

  implicit val ListPure = new Pure[List] {
    def pure[A](a: => A) = List(a)
  }

  implicit val OptionPure = new Pure[Option] {
    def pure[A](a: => A) = Some(a)
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

  implicit val IdentityApply: Apply[Identity] = FunctorBindApply

  implicit val Function0Apply: Apply[Function0] = FunctorBindApply

  implicit val ListApply: Apply[List] = FunctorBindApply

  implicit val OptionApply: Apply[Option] = FunctorBindApply
}

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  implicit val IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](a: Identity[A], f: A => Identity[B]) = f(a.value)
  }

  implicit val Function0Bind: Bind[Function0] = new Bind[Function0] {
    def bind[A, B](r: Function0[A], f: A => Function0[B]) = f(r.apply)
  }

  implicit val ListBind: Bind[List] = new Bind[List] {
    def bind[A, B](r: List[A], f: A => List[B]) = r flatMap f
  }

  implicit val OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](r: Option[A], f: A => Option[B]) = r flatMap f
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
    override def pure[A](a: => A) = p.pure(a)
    override def bind[A, B](a: M[A], f: A => M[B]) = b.bind(a, f)
  }
}

trait Traverse[T[_]] extends Functor[T] {
  def traverse[F[_], A, B](f: A => F[B], t: T[A])(implicit a: Applicative[F]): F[T[B]]

  import Scalaz._

  override def fmap[A, B](k: T[A], f: A => B) = traverse[Identity, A, B](f(_), k).value
}

object Traverse {
  import Scalaz._

  implicit val IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_], A, B](f: A => F[B], t: Identity[A])(implicit a: Applicative[F]) = a.fmap(f(t.value), (b: B) => b)
  }

  implicit val Function0Traverse: Traverse[Function0] = new Traverse[Function0] {
    def traverse[F[_], A, B](f: A => F[B], t: Function0[A])(implicit a: Applicative[F]) = a.fmap(f(t.apply), (b: B) => () => b)
  }

  implicit val ListTraverse: Traverse[List] = new Traverse[List] {
    def traverse[F[_], A, B](f: A => F[B], as: List[A])(implicit a: Applicative[F]): F[List[B]] =
      as.reverse.foldLeft[F[List[B]]](a.pure(Nil))((ys, x) => a(a.fmap(f(x), (a: B) => (b: List[B]) => a :: b), ys))
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

trait FoldLeft[-F[_]] {
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val IterableFoldLeft = new FoldLeft[Iterable] {
    def foldLeft[B, A](t: Iterable[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }
}

trait FoldRight[-F[_]] {
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
}

object FoldRight {
  implicit val ListFoldRight = new FoldRight[List] {
    def foldRight[A, B](t: List[A], b: B, f: (A, => B) => B) = IterableFoldRight.foldRight(t, b, f)
  }

  implicit val IterableFoldRight = new FoldRight[Iterable] {
    def foldRight[A, B](t: Iterable[A], b: B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
  }
}

trait Length[-L[_]] {
  def len[A](a: L[A]): Int
}

object Length {
  implicit val IterableLength: Length[Iterable] = new Length[Iterable] {
    def len[A](a: Iterable[A]) = {
      var n = 0
      val i = a.iterator
      while(i.hasNext) {
        n = n + 1
        i.next
      }

      n
    }
  }
}

trait Index[-I[_]] {
  def index[A](a: I[A], i: Int): Option[A]
}

object Index {
  implicit val IterableIndex: Index[Iterable] = new Index[Iterable] {
    def index[A](a: Iterable[A], i: Int) = if(i < 0) None else {
      var n = 0
      var k: Option[A] = None
      val it = a.iterator
      while(it.hasNext && k.isEmpty) {
        val z = it.next
        if(n == i) k = Some(z)
        n = n + 1
      }

      k
    }
  }
}

sealed trait Zero[+Z] {
  val zero: Z
}

object Zero {
  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }
}

trait Semigroup[S] {
  def append(s1: S, s2: => S): S
}

object Semigroup {
  def semigroup[S](f: (S, => S) => S) = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
  }

  implicit def ListSemigroup[A] = semigroup[List[A]](_ ::: _)
}

trait Monoid[M] extends Zero[M] with Semigroup[M]

object Monoid {
  implicit def monoid[M](implicit s: Semigroup[M], z: Zero[M]) = new Monoid[M] {
    def append(s1: M, s2: => M) = s append (s1, s2)
    val zero = z.zero
  }
}

trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

object Equal {
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  import Scalaz._

  implicit def IterableEqual[A](implicit ea: Equal[A]) = equal[Iterable[A]]((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 ≠ x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })
}

sealed trait Ordering {
  val toInt: Int
}
case object LT extends Ordering {
  val toInt = -1
}
case object EQ extends Ordering {
  val toInt = 0
}
case object GT extends Ordering {
  val toInt = 1
}

trait Order[-A] extends Equal[A] {
  def order(a1: A, a2: A): Ordering

  final def equal(a1: A, a2: A) = order(a1, a2) == EQ
}

object Order {
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }

  import Scalaz._

  implicit def IterableOrder[A](implicit oa: Order[A]): Order[Iterable[A]] = order((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = true
    var r: Ordering = EQ

    while(i1.hasNext && i2.hasNext && b) {
      val a1 = i1.next
      val a2 = i2.next

      val o = a1 ?|? a2
      if(o != EQ) {
        r = o
        b = false
      }
    }

    if(i1.hasNext)
      if(i2.hasNext)
        r
      else
        GT
    else
      LT
  })
}

trait Show[-A] {
  def show(a: A): List[Char]

  def shows(a: A) = show(a).mkString
}

object Show {
  def show[A](f: A => List[Char]) = new Show[A] {
    def show(a: A) = f(a)
  }

  import Scalaz._

  implicit def IterableShow[A](implicit sa: Show[A]) = show[Iterable[A]](as => {
    val i = as.iterator
    val k = new collection.mutable.ListBuffer[Char]
    k += '['
    while (i.hasNext) {
      val n = i.next
      k ++= n.show
      if (i.hasNext)
        k += ','
    }
    k += ']'
    k.toList
  })
}

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

sealed trait Identity[A] {
  val value: A

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
}

trait Identitys {
  implicit def IdentityTo[A](x: A): Identity[A] = new Identity[A] {
    val value = x
  }

  val unital = IdentityTo(())
}

sealed trait Digit {
  val toInt: Int
  def toLong = toInt.toLong

  def toChar = (toLong + 48).toChar

  override def equals(o: Any) = o.isInstanceOf[Digit] && o.asInstanceOf[Digit].toInt == toInt

  override def hashCode = toInt.hashCode

  override def toString = toInt.toString
}
case object _0 extends Digit {
  override val toInt = 0
}
case object _1 extends Digit {
  override val toInt = 1
}
case object _2 extends Digit {
  override val toInt = 2
}
case object _3 extends Digit {
  override val toInt = 3
}
case object _4 extends Digit {
  override val toInt = 4
}
case object _5 extends Digit {
  override val toInt = 5
}
case object _6 extends Digit {
  override val toInt = 6
}
case object _7 extends Digit {
  override val toInt = 7
}
case object _8 extends Digit {
  override val toInt = 8
}
case object _9 extends Digit {
  override val toInt = 9
}

trait Digits {
  val digits = List(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9)

  implicit def DigitLong(d: Digit): Long = d.toLong

  implicit def LongDigit(n: Long): Digit = n match {
    case 0L => _0
    case 1L => _1
    case 2L => _2
    case 3L => _3
    case 4L => _4
    case 5L => _5
    case 6L => _6
    case 7L => _7
    case 8L => _8
    case 9L => _9
    case _ => Math.abs(n) % 10L
  }
}

sealed trait CharW {
  val value: Char

  import Scalaz._

  def digit = digits find (_.toChar == value)
}

trait Chars {
  implicit def CharTo(c: Char): CharW = new CharW {
    val value = c
  }

  implicit def CharFrom(c: CharW): Char = c.value
}

object Scalaz extends MAs
              with    Identitys
              with    Digits
              with    Chars {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }
}

////

object Example {
  import Scalaz._

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

    // Digits
    println(List('1', 'x', '3') digits)

    // Traverse sequence
    {
      val g = List(Some(7): Option[Int], Some(9)).sequence
      println(g)
    }
  }
}
