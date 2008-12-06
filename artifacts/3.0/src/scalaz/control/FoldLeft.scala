// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Folding a function across an iterable environment from left to right.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait FoldLeft[F[_]] {
  /**
   * Fold the given function across the given environment.
   *
   * @param t The environment to fold across.
   * @param b The beginning value of the fold.
   * @param f The function to fold.
   */
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
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
object FoldLeft {
  /**
   * A fold-left for <code>scala.Option</code>.
   */
  implicit def OptionFoldLeft[A] = new FoldLeft[Option] {
    def foldLeft[B, A](t: Option[A], b: B, f: (B, A) => B) = t match {
      case None => b
      case Some(a) => f(b, a)
    }
  }

  /**
   * A fold-left for <code>scala.List</code>.
   */
  implicit def ListFoldLeft[A] = new FoldLeft[List] {
    def foldLeft[B, A](t: List[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  /**
   * A fold-left for <code>scala.Stream</code>.
   */
  implicit def StreamFoldLeft[A] = new FoldLeft[Stream] {
    def foldLeft[B, A](t: Stream[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  /**
   * A fold-left for <code>scala.Array</code>.
   */
  implicit def ArrayFoldLeft[A] = new FoldLeft[Array] {
    def foldLeft[B, A](t: Array[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  /**
   * A fold-left for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListFoldLeft[A] = new FoldLeft[NonEmptyList] {
    def foldLeft[B, A](t: NonEmptyList[A], b: B, f: (B, A) => B) = t.toList.foldLeft(b)(f)
  }
}
