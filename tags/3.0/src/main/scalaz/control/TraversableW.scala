// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Wraps <code>Traversable</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Traversable
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait TraversableW[T[_], A] {
  /**
   * The value for which there exists an instance of <code>Traversable</code>.
   */
  val v: T[A]

  /**
   * The <code>Traversable</code> instance.
   */
  val traversable: Traversable[T]

  /**
   * Maps each element of this traversable onto an action, evaluating from left to right and collecting the results.
   *
   * @param f The function to map on each element in the environment.
   */
  def traverse[F[_], B](f: A => F[B])(implicit a: Applicative[F]) = traversable.traverse[F, A, B](f, v)

  /**
   * Maps the given function across this traversable.
   */
  def >[B](f: A => B) = traversable.fmap(f, v)

  /**
   * Accumulates using the given monoidal append.
   */
  def -->>[B](f: A => B)(implicit m: Monoid[B]): B = {
    case class Acc[B, A](acc: B)

    implicit val AccApplicative = new Applicative[PartialType[Acc, B]#Apply] {
      def unit[A](a: A) = Acc[B, A](m.zero)
      def apply[A, X](f: PartialType[Acc, B]#Apply[A => X], a: PartialType[Acc, B]#Apply[A]) = Acc[B, X](m.append(f.acc, a.acc))
    }
    
    traverse[PartialType[Acc, B]#Apply, A](a => Acc(f(a))).acc
  }

  /**
   * Accumulates with identity.
   */
  def ->>(implicit m: Monoid[A]) = -->>(identity[A])
}

import validation.Validation

/**
 * Functions over traversable values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object TraversableW {
  /**
   * Constructs a traversable from the given value and implementation.
   */
  def traversable[T[_], A](a: T[A])(implicit t: Traversable[T]): TraversableW[T, A] = new TraversableW[T, A] {
    val v = a
    val traversable = t
  }

  /**
   * A traversable for <code>scala.Option</code>.
   */
  implicit def OptionTraversable[A](a: Option[A]): TraversableW[Option, A] = traversable[Option, A](a)

  /**
   * A traversable for <code>scala.List</code>.
   */
  implicit def ListTraversable[A](a: List[A]): TraversableW[List, A] = traversable[List, A](a)

  /**
   * A traversable for <code>scala.Stream</code>.
   */
  implicit def StreamTraversable[A](a: Stream[A]): TraversableW[Stream, A] = traversable[Stream, A](a)

  /**
   * A traversable for <code>scala.Array</code>.
   */
  implicit def ArrayTraversable[A](a: Array[A]): TraversableW[Array, A] = traversable[Array, A](a)

  /**
   * A traversable for <code>scala.Either</code>.
   */
  implicit def EitherTraversable[B, C](a: PartialType[Either, B]#Apply[C]): TraversableW[PartialType[Either, B]#Apply, C] = traversable[PartialType[Either, B]#Apply, C](a)

  /**
   * A traversable for <code>Validation</code>.
   */
  implicit def ValidationTraversable[B, C](a: PartialType[Validation, B]#Apply[C]): TraversableW[PartialType[Validation, B]#Apply, C] = traversable[PartialType[Validation, B]#Apply, C](a)
}
