// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Wraps <code>Semigroup</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Semigroup
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait SemigroupW[A] {
  /**
   * The value for which there exists an instance of <code>Semigroup</code>.
   */
  val v: A

  /**
   * The <code>Semigroup</code> instance.
   */
  val semigroup: Semigroup[A]

  /**
   * Appends the given value to this semigroup's value.
   */
  def |+|(a2: A): A = semigroup.append(v, a2)
}

import list.NonEmptyList

/**
 * Functions over semigroups.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object SemigroupW {
  /**
   * Constructs a semigroup from the given value and implementation.
   */
  implicit def semigroup[A](a: A)(implicit s: Semigroup[A]): SemigroupW[A] = new SemigroupW[A] {
    val v = a
    val semigroup = s
  }

  /**
   * A semigroup for <code>scala.Option</code>.
   */
  implicit def OptionSemigroup[A](a: Option[A]): SemigroupW[Option[A]] = semigroup(a)

  /**
   * A semigroup for <code>scala.List</code>.
   */
  implicit def ListSemigroup[A](a: List[A]): SemigroupW[List[A]] = semigroup(a)

  /**
   * A semigroup for <code>scala.Stream</code>.
   */
  implicit def StreamSemigroup[A](a: Stream[A]): SemigroupW[Stream[A]] = semigroup(a)

  /**
   * A semigroup for <code>scala.Array</code>.
   */
  implicit def ArraySemigroup[A](a: Array[A]): SemigroupW[Array[A]] = semigroup(a)

  /**
   * A semigroup for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListSemigroup[A](a: NonEmptyList[A]): SemigroupW[NonEmptyList[A]] = semigroup(a)
}
