// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Wraps <code>Each</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see scalaz.control.Each
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait EachW[E[_], A] {
  /**
   * The value for which there exists an instance of <code>Each</code>.
   */
  val v: E[A]

  /**
   * The <code>Each</code> instance.
   */
  val each: Each[E]

  /**
   * Executes the given side-effect for this each.
   */
  def !>(f: A => scala.Unit) { each.each(f, v) }

  /**
   * Executes the given side-effect for this each and the given each.
   */
  def !>[B](eb: E[B], f: (A, B) => scala.Unit) { !>((a: A) => each.each((b: B) => f(a, b), eb)) }

  /**
   * Executes the given side-effect for this each and the given each values.
   */
  def !>[B, C](eb: E[B], ec: E[C], f: (A, B, C) => scala.Unit) { !>((a: A) => each.each((b: B) => each.each((c: C) => f(a, b, c), ec), eb)) }

  /**
   * Executes the given side-effect for this each and the given each values.
   */
  def !>[B, C, D](eb: E[B], ec: E[C], ed: E[D], f: (A, B, C, D) => scala.Unit) { !>((a: A) => each.each((b: B) => each.each((c: C) => each.each((d: D) => f(a, b, c, d), ed), ec), eb)) }

  /**
   * Executes the given side-effect for this each and the given each values.
   */
  def !>[B, C, D, E$](eb: E[B], ec: E[C], ed: E[D], ee: E[E$], f: (A, B, C, D, E$) => scala.Unit) { !>((a: A) => each.each((b: B) => each.each((c: C) => each.each((d: D) => each.each((e: E$) => f(a, b, c, d, e), ee), ed), ec), eb)) }
}

import list.NonEmptyList

/**
 * Functions over each values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object EachW {
  /**
   * Constructs an each from the given value and implementation.
   */
  def each[E[_], A](ea: E[A])(implicit e: Each[E]): EachW[E, A] = new EachW[E, A] {
    val v = ea
    val each = e
  }

  /**
   * An each for <code>scala.Option</code>.
   */
  implicit def OptionEach[A](as: Option[A]): EachW[Option, A] = each[Option, A](as)

  /**
   * An each for <code>scala.List</code>.
   */
  implicit def ListEach[A](as: List[A]): EachW[List, A] = each[List, A](as)

  /**
   * An each for <code>scala.Stream</code>.
   */
  implicit def StreamEach[A](as: Stream[A]): EachW[Stream, A] = each[Stream, A](as)

  /**
   * An each for <code>scala.Array</code>.
   */
  implicit def ArrayEach[A](as: Array[A]): EachW[Array, A] = each[Array, A](as)

  /**
   * An each for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListEach[A](as: NonEmptyList[A]): EachW[NonEmptyList, A] = each[NonEmptyList, A](as)
}
