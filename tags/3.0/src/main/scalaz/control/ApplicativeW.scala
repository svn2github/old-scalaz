// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Wraps <code>Applicative</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Applicative
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ApplicativeW[AP[_], A] {
  /**
   * The value for which there exists an instance of <code>Applicative</code>.
   */
  val v: AP[A]

  /**
   * The <code>Applicative</code> instance.
   */
  val applicative: Applicative[AP]

  /**
   * Applies the given applicative function to this applicative instance.
   */
  def <*>[B](f: AP[A => B]) = applicative(f, v)

  /**
   * Applies the given applicative function to this applicative instance.
   */
  def <*>:[B](f: AP[A => B]) = <*>(f)

  /**
   * Maps the given function across this applicative instance.
   */
  def >[B](f: A => B) = <*>(applicative.unit(f))

  /**
   * Maps the given function across this applicative instance.
   */
  def <-:[B](f: A => B) = >(f)

  /**
   * Maps the given constant across this applicative instance.
   */
  def +>[B](b: B) = >(a => b)

  /**
   * Maps the given applicative constant across this applicative instance.
   */
  def *>[B](b: AP[B]) = |>(a => (b: B) => b, b)

  /**
   * Maps the given applicative constant across this applicative instance.
   */
  def <*[B](b: AP[B]) = |>(a => (b: B) => a, b)

  /**
   * Maps the given applicative constant across this applicative instance.
   */
  def <*|*>[B](b: AP[B]) = |>(a => (b: B) => (a, b), b)

  /**
   * Lifts the given arity-2 function into applicative.
   */
  def |>[B, C](f: A => B => C, b: AP[B]) = applicative(>(f), b)

  /**
   * Lifts the given arity-3 function into applicative.
   */
  def |>[B, C, D](f: A => B => C => D, b: AP[B], c: AP[C]) = applicative(applicative(>(f), b), c)

  /**
   * Lifts the given arity-4 function into applicative.
   */
  def |>[B, C, D, E](f: A => B => C => D => E, b: AP[B], c: AP[C], d: AP[D]) = applicative(applicative(applicative(>(f), b), c), d)

  /**
   * Lifts the given arity-5 function into applicative.
   */
  def |>[B, C, D, E, F](f: A => B => C => D => E => F, b: AP[B], c: AP[C], d: AP[D], e: AP[E]) = applicative(applicative(applicative(applicative(>(f), b), c), d), e)
}

import list.NonEmptyList
import validation.Validation

/**
 * Functions over applicative values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ApplicativeW {
  /**
   * Constructs an applicative from the given value and implementation.
   */
  def applicative[AP[_], A](a: AP[A])(implicit ap: Applicative[AP]): ApplicativeW[AP, A] = new ApplicativeW[AP, A] {
    val v = a
    val applicative = ap
  }

  /**
   * An applicative functor for <code>scala.Option</code>.
   */
  implicit def OptionApplicative[A](a: Option[A]): ApplicativeW[Option, A] = applicative[Option, A](a)

  /**
   * An applicative functor for <code>scala.List</code>.
   */
  implicit def ListApplicative[A](a: List[A]): ApplicativeW[List, A] = applicative[List, A](a)

  /**
   * An applicative functor for <code>scala.Stream</code>.
   */
  implicit def StreamApplicative[A](a: Stream[A]): ApplicativeW[Stream, A] = applicative[Stream, A](a)

  /**
   * An applicative functor for <code>scala.Array</code>.
   */
  implicit def ArrayApplicative[A](a: Array[A]): ApplicativeW[Array, A] = applicative[Array, A](a)

  /**
   * An applicative functor for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListApplicative[A](a: NonEmptyList[A]): ApplicativeW[NonEmptyList, A] = applicative[NonEmptyList, A](a)

  /**
   * An applicative functor for <code>scala.Function1</code>.
   */
  implicit def Function1Applicative[B, C](a: B => C): ApplicativeW[PartialType[Function1, B]#Apply, C] = applicative[PartialType[Function1, B]#Apply, C](a)

  /**
   * An applicative functor for <code>scala.Either</code>.
   */
  implicit def EitherApplicative[B, C](a: PartialType[Either, B]#Apply[C]): ApplicativeW[PartialType[Either, B]#Apply, C] = applicative[PartialType[Either, B]#Apply, C](a)

  /**
   * An applicative functor for <code>Validation</code>.
   */
  implicit def ValidationApplicative[E, A](a: PartialType[Validation, E]#Apply[A])(implicit s: Semigroup[E]): ApplicativeW[PartialType[Validation, E]#Apply, A] = applicative[PartialType[Validation, E]#Apply, A](a)
}
