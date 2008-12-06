// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

import OptionW._
import SemigroupW._

/**
 * Wraps <code>FoldLeft</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see FoldLeft
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait FoldLeftW[F[_], A] {
  /**
   * The value for which there exists an instance of <code>FoldLeft</code>.
   */
  val v: F[A]

  /**
   * The <code>FoldLeft</code> instance.
   */
  val foldleft: FoldLeft[F]

  /**
   * Folds across this iterable.
   */
  def foldl[B](b: B, f: (B, A) => B) = foldleft.foldLeft(v, b, f)

  /**
   * Folds across this iterable that must contain at least one element (hence, does not require a beginning value). If
   * this iterable is empty, then an error is thrown.
   */
  def foldl1(f: (A, A) => A) = foldl[Option[A]](None, (a1, a2) => Some(a1 match {
    case None => a2
    case Some(x) => f(a2, x)
  })).err("foldl1 on empty")

  /**
   * Sums the contents of this iterable using the given monoid with a left-fold.
   */
  def suml(implicit m: Monoid[A]) =
    foldl[A](m.zero, (b, a) => b |+| a)

  /**
   * Appends the given iterable to this iterable with a left-fold.
   */
  def appendl[B, F2[_]](b: B, f: (B, A) => B, fa2: F2[A])(implicit fd2: FoldLeft[F2]) = {
    val fl2 = FoldLeftW.foldLeft[F2, A](fa2)
    fl2 foldl (foldl(b, f), f)
  }

  /**
   * Returns the number of elements in this iterable.
   */
  val items = foldl[Int](0, (b, a) => b + 1)

  /**
   * Returns an <code>Each</code> instance for this iterable to execute side-effects per element.
   */
  val each: EachW[F, A] = EachW.each[F, A](v)(Each.FoldLeftEach[F](foldleft))

  /**
   * Executes the given side-effect for each element of this iterable.
   */
  def !>(f: A => scala.Unit) { each !> f }

  /**
   * Reverses this value using the given monad-empty-plus for reconstruction.
   */
  def rev[F2[_]](implicit m: MonadEmptyPlus[F2]): F2[A] =
    foldl[F2[A]](m.empty, (b, a) => a <+: MonadPlusW.monadPlus[F2, A](b))

  /**
   * Finds the maximum element in this iterable using the given order. This iterable must not be empty, otherwise an
   * error is thrown.
   */
  def max(implicit ord: Order[A]) =
    foldl1((x: A, y: A) => if(ord.order(x, y) == GT) x else y)

  /**
   * Finds the minimum element in this iterable using the given order. This iterable must not be empty, otherwise an
   * error is thrown.
   */
  def min(implicit ord: Order[A]) =
    foldl1((x: A, y: A) => if(ord.order(x, y) == LT) x else y)
}

import list.NonEmptyList

/**
 * Functions over fold-left values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FoldLeftW {
  /**
   * Constructs a fold-left from the given value and implementation.
   */
  def foldLeft[F[_], A](fa: F[A])(implicit fl: FoldLeft[F]): FoldLeftW[F, A] = new FoldLeftW[F, A] {
    val v = fa
    val foldleft = fl
  }

  /**
   * A fold-left for <code>scala.Option</code>.
   */
  implicit def OptionFoldLeft[A](as: Option[A]): FoldLeftW[Option, A] = foldLeft[Option, A](as)

  /**
   * A fold-left for <code>scala.List</code>.
   */
  implicit def ListFoldLeft[A](as: List[A]): FoldLeftW[List, A] = foldLeft[List, A](as)

  /**
   * A fold-left for <code>scala.Stream</code>.
   */
  implicit def StreamFoldLeft[A](as: Stream[A]): FoldLeftW[Stream, A] = foldLeft[Stream, A](as)

  /**
   * A fold-left for <code>scala.Array</code>.
   */
  implicit def ArrayFoldLeft[A](as: Array[A]): FoldLeftW[Array, A] = foldLeft[Array, A](as)

  /**
   * A fold-left for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListFoldLeft[A](as: NonEmptyList[A]): FoldLeftW[NonEmptyList, A] = foldLeft[NonEmptyList, A](as)
}
