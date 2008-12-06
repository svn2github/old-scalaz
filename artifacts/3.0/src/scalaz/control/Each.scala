// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Executes a side-effect through an environment.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Each[E[_]] {
  /**
   * Executes the given side-effect through the given environment.
   */
  def each[A](f: A => scala.Unit, fa: E[A]): scala.Unit  
}

import list.NonEmptyList

/**
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Each {
  /**
   * An each for <code>scala.Option</code>.
   */
  implicit val OptionEach = new Each[Option] {
    def each[A](f: A => scala.Unit, oa: Option[A]) = oa foreach f
  }

  /**
   * An each for <code>scala.List</code>.
   */
  implicit val ListEach = new Each[List] {
    def each[A](f: A => scala.Unit, oa: List[A]) = oa foreach f
  }

  /**
   * An each for <code>scala.Stream</code>.
   */
  implicit val StreamEach = new Each[Stream] {
    def each[A](f: A => scala.Unit, oa: Stream[A]) = oa foreach f
  }

  /**
   * An each for <code>scala.Array</code>.
   */
  implicit val ArrayEach = new Each[Array] {
    def each[A](f: A => scala.Unit, oa: Array[A]) = oa foreach f
  }

  /**
   * An each for <code>NonEmptyList</code>.
   */
  implicit val NonEmptyListEach = new Each[NonEmptyList] {
    def each[A](f: A => scala.Unit, oa: NonEmptyList[A]) = oa.toList foreach f    
  }

  /**
   * An each for <code>FoldLeft</code>.
   */
  implicit def FoldLeftEach[F[_]](implicit fd: FoldLeft[F]) = new Each[F] {
    def each[A](f: A => scala.Unit, oa: F[A]) = fd.foldLeft[scala.Unit, A](oa, (), (u, a) => f(a))       
  }

  /**
   * An each for <code>FoldRight</code>.
   */
  implicit def FoldRightEach[F[_]](implicit fd: FoldRight[F]) = new Each[F] {
    def each[A](f: A => scala.Unit, oa: F[A]) = fd.foldRight[A, scala.Unit](oa, (), (a, u) => f(a))
  }
}
