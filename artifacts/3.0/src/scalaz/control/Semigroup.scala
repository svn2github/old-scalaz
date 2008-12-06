// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Appends two elements.
 * 
 * <p>
 * All semigroup instances must satisfy 1 law:
 * <ol>
 * <li><strong>associativity</strong><br/><code>forall a b c. append(a, append(b, c)) == append(append(a, b), c)</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Semigroup[A] {
  /**
   * Appends the two given elements.
   */
  def append(a1: A, a2: A): A
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
object Semigroup {
  /**
   * Constructs a semigroup from the given value and implementation.
   */
  def semigroup[A](aa: (A, A) => A) = new Semigroup[A] {
    def append(a1: A, a2: A) = aa(a1, a2)  
  }

  /**
   * A semigroup for <code>String</code>.
   */
  implicit val StringSemigroup = new Semigroup[String] {
    def append(a1: String, a2: String) = a1 + a2
  }

  /**
   * A semigroup for <code>scala.Option</code>.
   */
  implicit def OptionSemigroup[A] = new Semigroup[Option[A]] {
    def append(a1: Option[A], a2: Option[A]) = if(a1.isDefined) a1 else a2
  }

  /**
   * A semigroup for <code>scala.List</code>.
   */
  implicit def ListSemigroup[A] = new Semigroup[List[A]] {
    def append(a1: List[A], a2: List[A]) = a1 ::: a2
  }

  /**
   * A semigroup for <code>scala.Stream</code>.
   */
  implicit def StreamSemigroup[A] = new Semigroup[Stream[A]] {
    def append(a1: Stream[A], a2: Stream[A]) = a1 append a2
  }

  /**
   * A semigroup for <code>scala.Array</code>.
   */
  implicit def ArraySemigroup[A] = new Semigroup[Array[A]] {
    def append(a1: Array[A], a2: Array[A]) = a1 ++ a2
  }

  /**
   * A semigroup for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListSemigroup[A] = new Semigroup[NonEmptyList[A]] {
    def append(a1: NonEmptyList[A], a2: NonEmptyList[A]) = a1.toList <::: a2
  }

  /**
   * A semigroup for <code>scala.Function1</code>.
   */
  implicit def Function1Semigroup[A, B](implicit sb: Semigroup[B]) = new Semigroup[Function1[A, B]] {
    def append(a1: Function1[A, B], a2: Function1[A, B]) = x => sb.append(a1(x), a2(x))
  }
}
