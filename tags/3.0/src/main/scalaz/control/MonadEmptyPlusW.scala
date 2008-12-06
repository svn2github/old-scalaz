// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

import MonadPlusW._
import MonadEmptyPlusW._

/**
 * Wraps <code>MonadEmptyPlus</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see MonadEmptyPlus
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait MonadEmptyPlusW[M[_], A] {
  /**
   * The value for which there exists an instance of <code>MonadEmptyPlus</code>.
   */
  val v: M[A]

  /**
   * The <code>MonadEmptyPlus</code> instance.
   */
  val monademptyplus: MonadEmptyPlus[M]

  /**
   * Drops from this environment while the given predicate satisfies. i.e. a generalised <code>dropWhile</code>.
   */
  def dropSelect(f: A => Boolean)(implicit p: Paramorphism[M]) =
    p.para[A, M[A]](v, monademptyplus.empty, (a, as, b) => if(f(a)) b else a <+: monadPlus[M, A](as)(monademptyplus))
}

/**
 * Functions over monad-empty-plus values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadEmptyPlusW {
  /**
   * Constructs a monad-empty-plus from the given value and implementation.
   */
  def monadEmptyPlus[M[_], A](ma: M[A])(implicit m: MonadEmptyPlus[M]): MonadEmptyPlusW[M, A] = new MonadEmptyPlusW[M, A] {
    val v = ma
    val monademptyplus = m
  }

  /**
   * A monad-empty-plus for <code>scala.Option</code>.
   */
  implicit def OptionMonadEmptyPlus[A](as: Option[A]): MonadEmptyPlusW[Option, A] = monadEmptyPlus[Option, A](as)

  /**
   * A monad-empty-plus for <code>scala.List</code>.
   */
  implicit def ListMonadEmptyPlus[A](as: List[A]): MonadEmptyPlusW[List, A] = monadEmptyPlus[List, A](as)

  /**
   * A monad-empty-plus for <code>scala.Stream</code>.
   */
  implicit def StreamMonadEmptyPlus[A](as: Stream[A]): MonadEmptyPlusW[Stream, A] = monadEmptyPlus[Stream, A](as)

  /**
   * A monad-empty-plus for <code>scala.Array</code>.
   */
  implicit def ArrayMonadEmptyPlus[A](as: Array[A]): MonadEmptyPlusW[Array, A] = monadEmptyPlus[Array, A](as)
}
