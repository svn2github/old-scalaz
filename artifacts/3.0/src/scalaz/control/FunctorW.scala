// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Wraps <code>Functor</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Functor
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait FunctorW[F[_], A] {
  /**
   * The value for which there exists an instance of <code>Functor</code>.
   */
  val v: F[A]

  /**
   * The <code>Functor</code> instance.
   */
  val functor: Functor[F]

  /**
   * Maps the given function across this functor.
   */
  def >[B](f: A => B): F[B]

  /**
   * Maps the given function across this functor.
   */
  def <-:[B](f: A => B) = >(f)
}

import list.NonEmptyList

/**
 * Functions over functors.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FunctorW {
  /**
   * Constructs a functor from the given value and implementation.
   */
  def functor[F[_], A](ft: F[A])(implicit f: Functor[F]): FunctorW[F, A] = new FunctorW[F, A] {
    val v = ft
    val functor = f
    def >[B](ff: A => B) = f.fmap(ff, ft)    
  }

  /**
   * A functor for <code>scala.Option</code>.
   */
  implicit def OptionFunctor[A](as: Option[A]): FunctorW[Option, A] = functor[Option, A](as)

  /**
   * A functor for <code>scala.List</code>.
   */
  implicit def ListFunctor[A](as: List[A]): FunctorW[List, A] = functor[List, A](as)

  /**
   * A functor for <code>scala.Stream</code>.
   */
  implicit def StreamFunctor[A](as: Stream[A]): FunctorW[Stream, A] = functor[Stream, A](as)

  /**
   * A functor for <code>scala.Array</code>.
   */
  implicit def ArrayFunctor[A](as: Array[A]): FunctorW[Array, A] = functor[Array, A](as)

  /**
   * A functor for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListFunctor[A](as: NonEmptyList[A]): FunctorW[NonEmptyList, A] = functor[NonEmptyList, A](as)

  /**
   * A functor for <code>scala.Function1</code>.
   */
  implicit def Function1Functor[B, C](as: B => C): FunctorW[PartialType[Function1, B]#Apply, C] = functor[PartialType[Function1, B]#Apply, C](as)

  /**
   * A functor for <code>scala.Either</code>.
   */
  implicit def EitherFunctor[B, C](as: PartialType[Either, B]#Apply[C]): FunctorW[PartialType[Either, B]#Apply, C] = functor[PartialType[Either, B]#Apply, C](as)
}
