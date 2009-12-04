package boo

trait PartialApply1Of2[T[_, _], A] {
  type Apply[B] = T[A, B]
  type Flip[B] = T[B, A]
}

trait Cofunctor[F[_]] {
 def comap[A, B](r: F[A], f: B => A): F[B]
}

object Cofunctor extends Function1s {
 implicit def CoFunction1Cofunctor[X]: Cofunctor[PartialApply1Of2[CoFunction1, X]#Flip] = new Cofunctor[PartialApply1Of2[CoFunction1, X]#Flip] {
   def comap[A, B](r: CoFunction1[A, X], f: B => A) = ((b: B) => r(f(b))).co
 }
}

trait Functor[F[_]] {
  def fmap[A, B](a: F[A], f: A => B): F[B]
}

object Functor {
 implicit def Function1Functor[X]: Functor[PartialApply1Of2[Function1, X]#Apply] = new Functor[PartialApply1Of2[Function1, X]#Apply] {
   def fmap[A, B](r: X => A, f: A => B) = f compose r
 }

  implicit def ListFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](r: List[A], f: A => B) = r map f
  }
}

trait CoFunction1[-T, +R] {
  def apply(t: T): R
}

sealed trait Function1W[T, R] {
  val value: T => R

  val co = new CoFunction1[T, R] {
      def apply(t: T) = value(t)
  }
}

trait Function1s {
  implicit def Function1To[T, R](f: T => R): Function1W[T, R] = new Function1W[T, R] {
    val value = f
  }

  implicit def Function1From[T, R](f: Function1W[T, R]): Function1[T, R] = f.value
}

sealed trait MA[M[_], A] {
 val v: M[A]

 def ∙[B](f: B => A)(implicit t: Cofunctor[M]) = t.comap(v, f)

 def ∘[B](f: A => B)(implicit t: Functor[M]) = t.fmap(v, f)
}

trait MAsLow {
  implicit def ima[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val v = a
  }
}

trait MAs extends Function1s with MAsLow {
  def ma[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val v = a
  }

  implicit def Function1ApplyMA[A, R](f: A => R): MA[PartialApply1Of2[Function1, A]#Apply, R] = ma[PartialApply1Of2[Function1, A]#Apply, R](f)

  implicit def CoFunction1FlipMA[A, R](f: CoFunction1[R, A]): MA[PartialApply1Of2[CoFunction1, A]#Flip, R] = ma[PartialApply1Of2[CoFunction1, A]#Flip, R](f)

  implicit def SeqMA[M[_] <: Seq[_], A](l: M[A]): MA[M, A] = ma[M, A](l)
}

////

object Example extends MAs {
 def main(args: Array[String]) {
  val f: Int => Int = (3+)

  val g = f ∘ f
  val h = f.co ∙ f

  println(g(7))
  println(h(7))
  println(List(1, 2, 3) ∘ g)
 }
}
