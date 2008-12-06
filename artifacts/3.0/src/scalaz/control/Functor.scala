// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Covariant function application in an environment. i.e. a covariant Functor.
 *
 * <p>
 * All functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == fmap(identity, a)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g. fmap(f compose g, a) == fmap(f, fmap(g, a))</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Functor[F[_]] {
  /**
   * Maps the given function across the given environement.
   */
  def fmap[A, B](f: A => B, fa: F[A]): F[B]
}

import list.NonEmptyList
import State.state

/**
 * Functions over covariant functors.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Functor {
  /**
   * A covariant functor for <code>scala.Option</code>.
   */
  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](f: A => B, oa: Option[A]) = oa map f
  }

  /**
   * A covariant functor for <code>scala.List</code>.
   */
  implicit val ListFunctor = new Functor[List] {
    def fmap[A, B](f: A => B, oa: List[A]) = oa map f
  }

  /**
   * A covariant functor for <code>scala.Stream</code>.
   */
  implicit val StreamFunctor = new Functor[Stream] {
    def fmap[A, B](f: A => B, oa: Stream[A]) = oa map f
  }

  /**
   * A covariant functor for <code>scala.Array</code>.
   */
  implicit val ArrayFunctor = new Functor[Array] {
    def fmap[A, B](f: A => B, oa: Array[A]) = oa map f
  }

  /**
   * A covariant functor for <code>NonEmptyList</code>.
   */
  implicit val NonEmptyListFunctor = new Functor[NonEmptyList] {
    def fmap[A, B](f: A => B, oa: NonEmptyList[A]) = oa map f
  }
  
  /**
   * A covariant functor for <code>scala.Function1</code>.
   */
  implicit def Function1Functor[A]: Functor[PartialType[Function1, A]#Apply] = new Functor[PartialType[Function1, A]#Apply] {
    def fmap[B, C](f: B => C, oa: PartialType[Function1, A]#Apply[B]): A => C = oa andThen f
  }

  /**
   * A covariant functor for <code>scala.Either</code>.
   */
  implicit def EitherFunctor[A]: Functor[PartialType[Either, A]#Apply] = new Functor[PartialType[Either, A]#Apply] {
    def fmap[B, C](f: B => C, ft: PartialType[Either, A]#Apply[B]) = ft.right map f
  }

  /**
   * A covariant functor for <code>State</code>.
   */
  implicit def StateFunctor[S]: Functor[PartialType[State, S]#Apply] = new Functor[PartialType[State, S]#Apply] {
    def fmap[B, C](f: B => C, ft: PartialType[State, S]#Apply[B]) = state((s: S) => {
      val sb = ft.state(s)
      (sb._1, f(sb._2))
    })
  }
}
