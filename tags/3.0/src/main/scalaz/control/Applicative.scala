// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Defines an applicative functor as described by McBride and Paterson in
 * <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.html">Applicative Programming with Effects</a>.
 *
 * <p>
 * All instances must satisfy 4 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == apply(unit(identity), a)</code></li>
 * <li><strong>composition</strong><br/><code>forall af ag a. apply(af, apply(ag, a)) == apply(apply(apply(unit(composition, af), ag), a))</code></li>
 * <li><strong>homomorphism</strong><br/><code>forall f a. apply(unit(f), unit(a)) == unit(f(a))</code></li>
 * <li><strong>interchange</strong><br/><code>forall af a. apply(af, unit(a)) == apply(unit(f => f(x)), af)</code></li>
 * </ol>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Applicative[AP[_]] extends Functor[AP] with Apply[AP] with Unit[AP] {
  /**
   * Defines the functor map for this applicative functor, which can be derived from the <code>apply</code> and
   * <code>unit</code> <em>(see Applicative Programming with Effects)</em>.
   */
  def fmap[A, B](f: A => B, a: AP[A]) = apply(unit(f), a)
}

import list.NonEmptyList
import list.NonEmptyList.nel
import validation.Validation
import validation.Validation.{success, fail}

/**
 * Functions over applicative values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Applicative {
  /**
   * Lifts the given value into unit using the given applicative.
   */
  def unit[A, U[_]](a: A)(implicit u: Applicative[U]) = u.unit(a)

  /**
   * An applicative functor for <code>scala.Option</code>.
   */
  implicit val OptionApplicative = new Applicative[Option] {
    def unit[A](a: A) = Some(a)
    def apply[A, B](f: Option[A => B], a: Option[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An applicative functor for <code>scala.List</code>.
   */
  implicit val ListApplicative = new Applicative[List] {
    def unit[A](a: A) = List(a)
    def apply[A, B](f: List[A => B], a: List[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An applicative functor for <code>scala.Stream</code>.
   */
  implicit val StreamApplicative = new Applicative[Stream] {
    def unit[A](a: A) = Stream(a)
    def apply[A, B](f: Stream[A => B], a: Stream[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An applicative functor for <code>scala.Array</code>.
   */
  implicit val ArrayApplicative = new Applicative[Array] {
    def unit[A](a: A): Array[A] = {
      val x = new Array[A](1)
      x(0) = a
      x
    }
    def apply[A, B](f: Array[A => B], a: Array[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An applicative functor for <code>NonEmptyList</code>.
   */
  implicit val NonEmptyListApplicative = new Applicative[NonEmptyList] {
    def unit[A](a: A) = nel(a)
    def apply[A, B](f: NonEmptyList[A => B], a: NonEmptyList[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An applicative functor for <code>scala.Function1</code>.
   */
  implicit def Function1Applicative[X] = new Applicative[PartialType[Function1, X]#Apply] {
    def unit[A](a: A) = (x: X) => a
    def apply[A, B](f: PartialType[Function1, X]#Apply[A => B], a: PartialType[Function1, X]#Apply[A]) = (x: X) => f(x)(a(x))
  }

  /**
   * An applicative functor for <code>scala.Either</code>.
   */
  implicit def EitherApplicative[X] = new Applicative[PartialType[Either, X]#Apply] {
    def unit[A](a: A) = Right(a)
    def apply[A, B](f: PartialType[Either, X]#Apply[A => B], a: PartialType[Either, X]#Apply[A]) = f.right.flatMap(f => a.right.map(f(_)))
  }

  /**
   * An applicative functor for <code>Validation</code>.
   */
  implicit def ValidationApplicative[E](implicit s: Semigroup[E]): Applicative[PartialType[Validation, E]#Apply] = new Applicative[PartialType[Validation, E]#Apply] {
    def unit[A](a: A) = success[E, A](a)
    def apply[A, B](f: PartialType[Validation, E]#Apply[A => B], a: PartialType[Validation, E]#Apply[A]) = (f.either, a.either) match {
      case (Right(f), Right(a)) => success[E, B](f(a))
      case (Right(f), Left(e)) => fail[E, B](e)
      case (Left(e), Right(_)) => fail[E, B](e)
      case (Left(e1), Left(e2)) => fail[E, B](s.append(e1, e2))
    }
  }
}
