// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * A monad with plus.
 *
 * <p>
 * All monad-plus instances must satisfy the monad laws and 1 additional law:
 * <ol>
 * <li><strong>associativity</strong><br/><code>forall a b c. plus(a, plus(a, b)) == plus(plus(a, b), c)</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MonadPlus[M[_]] extends Monad[M] with Plus[M]

import list.NonEmptyList
import list.NonEmptyList.nel

/**
 * Functions over monad-plus values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadPlus {
  import Monad._
  import Functor._

  /**
   * Lifts the given value into unit using the given monad-plus.
   */
  def unit[A, U[_]](a: A)(implicit u: MonadPlus[U]) = u.unit(a)

  /**
   * A monad-plus for <code>scala.Option</code>.
   */
  implicit val OptionMonadPlus = new MonadPlus[Option] {
    def plus[A](a1: Option[A], a2: Option[A]) = if(a1.isEmpty) a2 else a1
    def unit[A](a: A) = Some(a) // WTF? Calling OptionMonad.unit(a) crashes the type checker
    def bind[A, B](f: A => Option[B], o: Option[A]) = OptionMonad.bind(f, o)
  }

  /**
   * A monad-plus for <code>scala.List</code>.
   */
  implicit val ListMonadPlus = new MonadPlus[List] {
    def plus[A](a1: List[A], a2: List[A]) = a1 ::: a2
    def unit[A](a: A) = a :: Nil
    def bind[A, B](f: A => List[B], o: List[A]) = ListMonad.bind(f, o)
  }

  /**
   * A monad-plus for <code>scala.Stream</code>.
   */
  implicit val StreamMonadPlus = new MonadPlus[Stream] {
    def plus[A](a1: Stream[A], a2: Stream[A]) = a1 append a2
    def unit[A](a: A) = StreamMonad.unit(a)
    def bind[A, B](f: A => Stream[B], o: Stream[A]) = StreamMonad.bind(f, o)
  }

  /**
   * A monad-plus for <code>scala.Array</code>.
   */
  implicit val ArrayMonadPlus = new MonadPlus[Array] {
    def plus[A](a1: Array[A], a2: Array[A]) = a1 ++ a2
    def unit[A](a: A) = ArrayMonad.unit(a)
    def bind[A, B](f: A => Array[B], o: Array[A]) = ArrayMonad.bind(f, o)
  }

  /**
   * A monad-plus for <code>NonEmptyList</code>.
   */
  implicit val NonEmptyListMonadPlus = new MonadPlus[NonEmptyList] {
    def plus[A](a1: NonEmptyList[A], a2: NonEmptyList[A]) = a1.toList <::: a2
    def unit[A](a: A) = nel(a)
    def bind[A, B](f: A => NonEmptyList[B], o: NonEmptyList[A]) = NonEmptyListMonad.bind(f, o)
  }

  /**
   * A monad-plus for <code>scala.Either</code>.
   */
  implicit def EitherMonadPlus[X] = new MonadPlus[PartialType[Either, X]#Apply] {
    def plus[A](a1: PartialType[Either, X]#Apply[A], a2: PartialType[Either, X]#Apply[A]) = if(a1.isRight) a1 else a2
    def unit[A](a: A) = EitherMonad.unit(a)
    def bind[A, B](f: A => PartialType[Either, X]#Apply[B], o: PartialType[Either, X]#Apply[A]) = EitherMonad.bind(f, o)
  }
}
