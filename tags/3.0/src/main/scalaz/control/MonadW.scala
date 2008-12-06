// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

import Function.curried
import Monad.sequence
import FunctorW._

/**
 * Wraps <code>Monad</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Monad
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait MonadW[M[_], A] {
  /**
   * The value for which there exists an instance of <code>Monad</code>.
   */
  val v: M[A]

  /**
   * The <code>Monad</code> instance.
   */
  val monad: Monad[M]

  /**
   * Maps the given function across this monad.
   */
  def >[B](f: A => B) = monad.fmap(f, v)

  /**
   * Binds the given function across this monad.
   */
  def >>=[B](f: A => M[B]) = monad.bind(f, v)

  /**
   * Constant bind the given value across this monad.
   */
  def >>[B](mb: M[B]) = monad.bind((x: A) => mb, v)

  /**
   * Binds the given function across this monad.
   */
  def =<<:[B](f: A => M[B]) = >>=(f)

  /**
   * Constant bind the given value across this monad.
   */
  def <<:[B](mb: M[B]) = >>(mb)

  /**
   * Function application within the monad environment.
   */
  def ap[B](mf: M[A => B]) = monad.bind((a: A) => monad.fmap((f: A => B) => f(a), mf), v)

  /**
   * Binds the given function across this monad and the given monad value.
   */
  def >>=[B, C](mb: M[B], f: (A, B) => C) = MonadW.monad[M, B](mb)(monad) ap monad.fmap[A, B => C](curried(f), v)

  /**
   * Binds the given function across this monad and the given monad values.
   */
  def >>=[B, C, D](mb: M[B], mc: M[C], f: (A, B, C) => D) = MonadW.monad[M, C](mc)(monad) ap >>=[B, C => D](mb, (a, b) => c => f(a, b, c))

  /**
   * Binds the given function across this monad and the given monad values.
   */
  def >>=[B, C, D, E](mb: M[B], mc: M[C], md: M[D], f: (A, B, C, D) => E) = MonadW.monad[M, D](md)(monad) ap >>=[B, C, D => E](mb, mc, (a, b, c) => d => f(a, b, c, d))

  /**
   * Binds the given function across this monad and the given monad values.
   */
  def >>=[B, C, D, E, F](mb: M[B], mc: M[C], md: M[D], me: M[E], f: (A, B, C, D, E) => F) = MonadW.monad[M, E](me)(monad) ap >>=[B, C, D, E => F](mb, mc, md, (a, b, c, d) => e => f(a, b, c, d, e))

  /**
   * Sequence this monad the given number of times.
   */
  def replicate[T[_]](n: Int)(implicit m: MonadEmptyPlus[T], f: FoldRight[T]) = sequence[A, M, T, T](MonadEmptyPlus.replicate[M[A], T](n, v))(monad, f, m)

  /**
   * Map the given function across this monad by sequencing.
   */
  def mapM[B, MP[_], M2[_]](f: A => M2[B])(implicit m: Monad[M2], fd: FoldRight[M], mp: MonadEmptyPlus[MP]) = sequence[B, M2, M, MP](functor[M, A](v)(monad) > f)
}

import list.NonEmptyList

/**
 * Functions over monad values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadW {
  /**
   * Constructs a monad from the given value and implementation.
   */
  def monad[M[_], A](ma: M[A])(implicit m: Monad[M]): MonadW[M, A] = new MonadW[M, A] {
    val v = ma
    val monad = m
  }

  /**
   * A monad for <code>scala.Option</code>.
   */
  implicit def OptionMonad[A](as: Option[A]): MonadW[Option, A] = monad[Option, A](as)

  /**
   * A monad for <code>scala.List</code>.
   */
  implicit def ListMonad[A](as: List[A]): MonadW[List, A] = monad[List, A](as)

  /**
   * A monad for <code>scala.Stream</code>.
   */
  implicit def StreamMonad[A](as: Stream[A]): MonadW[Stream, A] = monad[Stream, A](as)

  /**
   * A monad for <code>scala.Array</code>.
   */
  implicit def ArrayMonad[A](as: Array[A]): MonadW[Array, A] = monad[Array, A](as)

  /**
   * A monad for <code>NonEmptyList</code>.
   */
  implicit def NonEmptyListMonad[A](as: NonEmptyList[A]): MonadW[NonEmptyList, A] = monad[NonEmptyList, A](as)

  /**
   * A monad for <code>scala.Function1</code>.
   */
  implicit def Function1Monad[B, C](as: B => C): MonadW[PartialType[Function1, B]#Apply, C] = monad[PartialType[Function1, B]#Apply, C](as)

  /**
   * A monad for <code>scala.Either</code>.
   */
  implicit def EitherMonad[B, C](as: PartialType[Either, B]#Apply[C]): MonadW[PartialType[Either, B]#Apply, C] = monad[PartialType[Either, B]#Apply, C](as)
}
