// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

import OptionW._
import SemigroupW._
import MonadPlusW._

/**
 * Wraps <code>FoldRight</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see FoldRight
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait FoldRightW[F[_], A] {
  /**
   * The value for which there exists an instance of <code>FoldRight</code>.
   */
  val v: F[A]

  /**
   * The <code>FoldRight</code> instance.
   */
  val foldright: FoldRight[F]

  /**
   * Folds across this iterable.
   */
  def foldr[B](b: B, f: (A, => B) => B) = foldright.foldRight(v, b, f)

  /**
   * Folds across this iterable that must contain at least one element (hence, does not require a beginning value). If
   * this iterable is empty, then an error is thrown.
   */
  def foldr1(f: (A, => A) => A) = foldr[Option[A]](None, (a1, a2) => Some(a2 match {
    case None => a1
    case Some(x) => f(a1, x)
  })).err("foldr1 on empty")

  /**
   * Sums the contents of this iterable using the given monoid with a right-fold.
   */
  def sumr(implicit m: Monoid[A]) =
    foldr[A](m.zero, (a, b) => a |+| b)

  /**
   * Appends the given iterable to this iterable with a right-fold.
   */
  def appendr[B, F2[_]](b: B, f: (A, => B) => B, fa2: F2[A])(implicit fd2: FoldRight[F2])  = {
    val fr2 = FoldRightW.foldRight[F2, A](fa2)
    foldr(fr2 foldr(b, f), f)
  }

  /**
   * Returns a <code>Stream</code> representation of this iterable.
   */
  val stream = foldr[Stream[A]](Stream.empty, Stream.cons(_, _))

  /**
   * Returns the element at the given index of this iterable.
   */
  def !(n: Int) = stream(n)

  /**
   * Returns an <code>Each</code> instance for this iterable to execute side-effects per element.
   */
  val each = EachW.each[F, A](v)(Each.FoldRightEach[F](foldright))

  /**
   * Intercalates the given separator through this iterable. e.g.
   * <p>
   * <code>[1, 2, 3] intercalate ([7, 8, 9])</code> gives
   * </p>
   * <p>
   * <code>[1, 7, 8, 9, 2, 7, 8, 9, 3]</code>.
   * </p>
   */
  def intercalate[M[_]](sep: M[A])(implicit m: MonadEmptyPlus[M]) = {
    implicit val sepm: MonadPlusW[M, A] = monadPlus[M, A](sep)
    foldr[(M[A], Boolean)]((m.empty, true), (a, b) => (a <+: monadPlus[M, A]((if(b._2) monadPlus[M, A](m.empty) else sepm) <+> b._1), false))._1
  }

  /**
   * Returns <code>true</code> if any elements of this iterable satisfy the given predicate, otherwise
   * <code>false</code>.
   */
  def any(p: A => Boolean) = foldr[Boolean](false, p(_) || _)

  /**
   * Returns <code>true</code> if all of the elements of this iterable satisfy the given predicate, otherwise
   * <code>false</code>.
   */
  def all(p: A => Boolean) = foldr[Boolean](true, p(_) && _)

  /**
   * Returns <code>true</code> if this iterable is empty, otherwise <code>false</code>.
   */
  val isEmpty = all(a => false)

  /**
   * Returns elements of this iterable that satisfy the given predicate. i.e. a generalised <code>filter</code>.
   */
  def select[M[_]](p: A => Boolean)(implicit m: MonadEmptyPlus[M]) =
    foldr[M[A]](m.empty, (a, as) => {
      implicit val asm: MonadPlusW[M, A] = monadPlus[M, A](as)
      if(p(a)) a <+: asm else as
    })

  /**
   * Returns elements of this iterable while satisfying the given predicate. i.e. a generalised <code>takeWhile</code>.
   */
  def selectWhile[M[_]](p: A => Boolean)(implicit m: MonadEmptyPlus[M]) =
    foldr[M[A]](m.empty, (a, as) => {
      implicit val asm: MonadPlusW[M, A] = monadPlus[M, A](as)
      if(p(a)) a <+: asm else m.empty
    })

  /**
   * Splits this iterable into a list of lists, such that each contained list has elements that alternatively
   * satisfiy the predicate.
   * e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] splitWith (_ % 2 == 0)</code> yields
   * </p>
   * <p>
   * <code>[[2, 6, 8], [9], [6], [7, 3, 5], [8, 6], [9]]</code>
   * </p>
   */
  def splitWith(p: A => Boolean) = foldr[(List[List[A]], Option[Boolean])]((Nil, None), (a, b) => {
      val pa = p(a)
      (b match {
        case (_, None) => List(List(a))
        case (x, Some(q)) => if(pa == q) (a :: x.head) :: x.tail else List(a) :: x
      }, Some(pa))
    })._1

  /**
   * Splits this iterable into a list of lists such that each contained list has elements that satisfy the predicate,
   * with each list split by elements that do not satisfy the predicate. e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] selectSplit (_ % 2 == 0)</code> yields
   * </p>
   * <p>
   * <code>[[2, 6, 8], [6], [8, 6]]</code>
   * </p>
   */
  def selectSplit(p: A => Boolean) = foldr[(List[List[A]], Boolean)]((Nil, false), (a, xb) => xb match {
      case (x, b) => {
        val pa = p(a)
        (if(pa) if(b) (a :: x.head) :: x.tail else List(a) :: x else x, pa)
      }
    })._1

  /**
   * Filters the given monadic predicate over the given iterable value.
   */
  def filterM[M[_], MP[_]](f: A => M[Boolean])(implicit m: Monad[M], mp: MonadEmptyPlus[MP]) =
    foldright.foldRight[A, M[MP[A]]](v, m.unit(mp.empty), (a, b) => m.bind((c: Boolean) =>
            m.fmap((ys: MP[A]) => if(c) mp.plus(mp.unit(a), ys) else ys, b), f(a)))

  /**
   * Folds left inside a monad over the given iterable value.
   */
  def foldM[B, M[_]](b: B, f: (A, B) => M[B])(implicit m: Monad[M]) =
    foldright.foldRight[A, M[B]](v, m.unit(b), (a, b) => m.bind(f(a, (_: B)), b))
}

import list.NonEmptyList

/**
 * Functions over fold-right values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FoldRightW {
  /**
   * Constructs a fold-right from the given value and implementation.
   */
  def foldRight[F[_], A](fa: F[A])(implicit fr: FoldRight[F]): FoldRightW[F, A] = new FoldRightW[F, A] {
    val v = fa

    val foldright = fr
  }
  
  /**
   * A fold-right for <code>scala.Option</code>.
   */
  implicit def OptionFoldRight[A](as: Option[A]): FoldRightW[Option, A] = foldRight[Option, A](as)

  /**
   * A fold-right for <code>scala.List</code>.
   */
  implicit def ListFoldRight[A](as: List[A]): FoldRightW[List, A] = foldRight[List, A](as)

  /**
   * A fold-right for <code>scala.Stream</code>.
   */
  implicit def StreamFoldRight[A](as: Stream[A]): FoldRightW[Stream, A] = foldRight[Stream, A](as)

  /**
   * A fold-right for <code>scala.Array</code>.
   */
  implicit def ArrayFoldRight[A](as: Array[A]): FoldRightW[Array, A] = foldRight[Array, A](as)

  /**
   * A fold-right for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListFoldRight[A](as: NonEmptyList[A]): FoldRightW[NonEmptyList, A] = foldRight[NonEmptyList, A](as)
}
