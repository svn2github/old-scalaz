// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Folding a function across an iterable environment from right to left.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait FoldRight[F[_]] {
  /**
   * Fold the given function across the given environment.
   *
   * @param t The environment to fold across.
   * @param b The beginning value of the fold.
   * @param f The function to fold.
   */
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
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
object FoldRight {
  /**
   * A fold-right for <code>scala.Option</code>.
   */
  implicit def OptionFoldRight[A] = new FoldRight[Option] {
    def foldRight[A, B](t: Option[A], b: B, f: (A, => B) => B) = t match {
      case None => b
      case Some(a) => f(a, b)
    }
  }

  /**
   * A fold-right for <code>scala.List</code>.
   */
  implicit def ListFoldRight[A] = new FoldRight[List] {
    def foldRight[A, B](t: List[A], b: B, f: (A, => B) => B): B = t match {
      case Nil => b
      case x :: xs => f(x, foldRight(xs, b, f))
    }
  }

  /**
   * A fold-right for <code>scala.Stream</code>.
   */
  implicit def StreamFoldRight[A] = new FoldRight[Stream] {
    def foldRight[A, B](t: Stream[A], b: B, f: (A, => B) => B): B =
      if(t.isEmpty)
        b
      else
        f(t.head, foldRight(t.tail, b, f))
  }

  /**
   * A fold-right for <code>scala.Array</code>.
   */
  implicit def ArrayFoldRight[A] = new FoldRight[Array] {
    def foldRight[A, B](t: Array[A], b: B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
  }

  /**
   * A fold-right for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListFoldRight[A] = new FoldRight[NonEmptyList] {
    def foldRight[A, B](t: NonEmptyList[A], b: B, f: (A, => B) => B): B = t.toList.foldRight(b)(f(_, _))
  }
}
