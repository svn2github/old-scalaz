// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

import MonadPlusW._

/**
 * Wraps <code>Paramorphism</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Paramorphism
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ParamorphismW[P[_], A] {
  /**
   * The value for which there exists an instance of <code>Paramorphism</code>.
   */
  val v: P[A]

  /**
   * The <code>Paramorphism</code> instance.
   */
  val paramorphism: Paramorphism[P]

  /**
   * Performs a paramorphic fold across this iterable.
   */
  def para[B](b: B, f: (=> A, => P[A], B) => B) = paramorphism.para(v, b, f)

  /**
   * Splits this iterable using the given predicate. The left of the tuple contains the first elements that satisfy
   * the given predicate.
   * e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] |- (_ % 2 == 0)</code> yields
   * </p>
   * <p>
   * <code>([2, 6, 8],[9, 6, 7, 3, 5, 8, 6, 9])</code>
   * </p>
   */
  def |-(p: A => Boolean)(implicit m: MonadEmptyPlus[P]) =
    para[(P[A], P[A])]((m.empty, m.empty), (a, as, asbs) =>
            if(p(a)) (a <+: monadPlus[P, A](asbs._1)(m), asbs._2)
            else (m.empty, a <+: monadPlus[P, A](as)(m)))
  /**
   * Splits this iterable using the given predicate. The left of the tuple contains the first elements that do not
   * satisfy the given predicate.
   * e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] !- (_ % 2 != 0)</code> yields
   * </p>
   * <p>
   * <code>([2, 6, 8],[9, 6, 7, 3, 5, 8, 6, 9])</code>
   * </p>
   */
  def !-(p: A => Boolean)(implicit m: MonadEmptyPlus[P]) = |-(!p(_))
}

import list.NonEmptyList

/**
 * Functions over paramorphisms.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ParamorphismW {
  /**
   * Constructs a paramorphism from the given value and implementation.
   */
  def paramorphism[P[_], A](pa: P[A])(implicit p: Paramorphism[P]): ParamorphismW[P, A] = new ParamorphismW[P, A] {
    val v = pa
    val paramorphism = p
  }

  /**
   * A paramorphism for <code>scala.Option</code>.
   */
  implicit def OptionParamorphism[A](a: Option[A]): ParamorphismW[Option, A] = paramorphism[Option, A](a)

  /**
   * A paramorphism for <code>scala.Option</code>.
   */
  implicit def ListParamorphism[A](a: List[A]): ParamorphismW[List, A] = paramorphism[List, A](a)

  /**
   * A paramorphism for <code>scala.Option</code>.
   */
  implicit def StreamParamorphism[A](a: Stream[A]): ParamorphismW[Stream, A] = paramorphism[Stream, A](a)

  /**
   * A paramorphism for <code>scala.Option</code>.
   */
  implicit def NonEmptyListParamorphism[A](a: NonEmptyList[A]): ParamorphismW[NonEmptyList, A] = paramorphism[NonEmptyList, A](a)
}
