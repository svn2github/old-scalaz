// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Abstract a model that sequences computation through an environment.
 *
 * <p>
 * All monad instances must satisfy 3 laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>forall a f. f(a) == bind(f, unit(a))</code></li>
 * <li><strong>right identity</strong><br/><code>forall a. a == bind(x => unit(x), a)</code></li>
 * <li><strong>associativity</strong><br/><code>forall a f g. bind(x => bind(g, f(x)), a) == bind(g, bind(f, a))</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Monad[M[_]] extends Applicative[M] with Bind[M] with Unit[M] {
  /**
   * Apply the given function in the given value through the environment.
   */
  def apply[A, B](f: M[A => B], a: M[A]): M[B] = bind((aa: A) => bind((t: A => B) => unit(t(aa)), f), a)
}

import Functor._
import list.NonEmptyList
import list.NonEmptyList.nel
import State.state

/**
 * Functions over monads.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Monad {
  /**
   * Lifts the given value into unit using the given monad.
   */
  def unit[A, U[_]](a: A)(implicit u: Monad[U]) = u.unit(a)

  /**
   * A monad for <code>scala.Option</code>.
   */
  implicit val OptionMonad = new Monad[Option] {
    def unit[A](a: A) = Some(a)
    def bind[A, B](f: A => Option[B], a: Option[A]) = a flatMap f
  }

  /**
   * A monad for <code>scala.List</code>.
   */
  implicit val ListMonad = new Monad[List] {
    def unit[A](a: A) = a :: Nil
    def bind[A, B](f: A => List[B], a: List[A]) = a flatMap f
  }

  /**
   * A monad for <code>scala.Stream</code>.
   */
  implicit val StreamMonad = new Monad[Stream] {
    def unit[A](a: A) = Stream.cons(a, Stream.empty)
    def bind[A, B](f: A => Stream[B], a: Stream[A]) = a flatMap f
  }

  /**
   * A monad for <code>scala.Array</code>.
   */
  implicit val ArrayMonad = new Monad[Array] {
    def unit[A](a: A) = {
      val x = new Array[A](1)
      x(0) = a
      x
    }
    def bind[A, B](f: A => Array[B], a: Array[A]) = a flatMap f
  }

  /**
   * A monad for <code>NonEmptyList</code>.
   */
  implicit val NonEmptyListMonad = new Monad[NonEmptyList] {
    def unit[A](a: A) = nel(a)
    def bind[A, B](f: A => NonEmptyList[B], a: NonEmptyList[A]) = a flatMap f
  }

  /**
   * A monad for <code>scala.Function1</code>.
   */
  implicit def Function1Monad[X]: Monad[PartialType[Function1, X]#Apply] = new Monad[PartialType[Function1, X]#Apply] {
    def unit[A](a: A) = (x: X) => a
    def bind[A, B](f: A => X => B, a: X => A) = (x: X) => f(a(x))(x)
  }

  /**
   * A monad for <code>scala.Either</code>.
   */
  implicit def EitherMonad[X]: Monad[PartialType[Either, X]#Apply] = new Monad[PartialType[Either, X]#Apply] {
    def unit[A](a: A) = Right(a)
    def bind[A, B](f: A => PartialType[Either, X]#Apply[B], a: PartialType[Either, X]#Apply[A]) = a.right flatMap f
  }

  /**
   * A monad for <code>State</code>.
   */
  implicit def StateMonad[S]: Monad[PartialType[State, S]#Apply] = new Monad[PartialType[State, S]#Apply] {
    def unit[A](a: A) = state((s: S) => (s, a))
    def bind[A, B](f: A => PartialType[State, S]#Apply[B], a: PartialType[State, S]#Apply[A]) =
      state[S, B]((s: S) => {
        val t = a.state(s)
        f(t._2).state(t._1)
      })
  }

  /**
   * Removes one level of monadic structure, projecting its bound argument into the outer level of the monad. Equivalent
   * to <code>bind(identity, ma)</code>.
   */
  def join[M[_], A](ma: M[M[A]])(implicit m: Monad[M]): M[A] = m.bind(identity[M[A]], ma)

  /**
   * If the given condition is <code>true</code>, then execute the given side-effect, otherwise perform no side-effect.
   */
  def when[M[_]](c: Boolean, s: M[() => scala.Unit])(implicit m: Monad[M]): M[scala.Unit] =
    if(c) m.fmap((f: () => scala.Unit) => f(), s) else m.unit(())

  /**
   * If the given condition is <code>false</code>, then execute the given side-effect, otherwise perform no side-effect.
   */
  def unless[M[_]](c: Boolean, s: M[() => scala.Unit])(implicit m: Monad[M]): M[scala.Unit] =
    if(c) m.unit(()) else m.fmap((f: () => scala.Unit) => f(), s)

  /**
   * Sequences the given iterable of monad values to produce a monad value of a container.
   */
  def sequence[A, M[_], F[_], MP[_]](ms: F[M[A]])(implicit m: Monad[M], fr: FoldRight[F], mp: MonadEmptyPlus[MP]): M[MP[A]] =
    fr.foldRight[M[A], M[MP[A]]](ms, m.unit(mp.empty), (a, b) => m.bind((x: A) => m.fmap((xs: MP[A]) => mp.plus(mp.unit(x), xs), b), a))
}
