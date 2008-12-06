// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Wraps <code>MonadPlus</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see MonadPlus
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait MonadPlusW[M[_], A] {
  /**
   * The value for which there exists an instance of <code>MonadPlus</code>.
   */
  val v: M[A]

  /**
   * The <code>MonadPlus</code> instance.
   */
  val monadplus: MonadPlus[M]

  /**
   * Maps the given function across this monad-plus.
   */
  def >[B](f: A => B) = monadplus.fmap(f, v)

  /**
   * Plus this environment with the given monad-plus environment.
   */
  def <+>(a2: M[A]) = monadplus.plus(v, a2)

  /**
   * Plus this environment with the unit of the given value.
   */
  def <+(a2: A) = monadplus.plus(v, monadplus.unit(a2))

  /**
   * Plus this environment with the unit of the given value.
   */
  def <+:(a2: A): M[A] = monadplus.plus(monadplus.unit(a2), v)
}

import list.NonEmptyList

/**
 * Functions over monad-plus values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadPlusW {
  /**
   * Constructs a monad-plus from the given value and implementation.
   */
  def monadPlus[M[_], A](ma: M[A])(implicit m: MonadPlus[M]): MonadPlusW[M, A] = new MonadPlusW[M, A] {
    val v = ma
    val monadplus = m
  }

  /**
   * A monad-plus for <code>scala.Option</code>.
   */
  implicit def OptionMonadPlus[A](as: Option[A]): MonadPlusW[Option, A] = monadPlus[Option, A](as)

  /**
   * A monad-plus for <code>scala.List</code>.
   */
  implicit def ListMonadPlus[A](as: List[A]): MonadPlusW[List, A] = monadPlus[List, A](as)

  /**
   * A monad-plus for <code>scala.Stream</code>.
   */
  implicit def StreamMonadPlus[A](as: Stream[A]): MonadPlusW[Stream, A] = monadPlus[Stream, A](as)

  /**
   * A monad-plus for <code>scala.Array</code>.
   */
  implicit def ArrayMonadPlus[A](as: Array[A]): MonadPlusW[Array, A] = monadPlus[Array, A](as)

  /**
   * A monad-plus for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListMonadPlus[A](as: NonEmptyList[A]): MonadPlusW[NonEmptyList, A] = monadPlus[NonEmptyList, A](as)
}
