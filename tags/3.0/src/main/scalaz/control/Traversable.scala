// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

import Id.id

/**
 * Defines a traversable type as described by McBride and Paterson in
 * <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.html">Applicative Programming with Effects</a>. Also see
 * <a href="http://citeseer.ist.psu.edu/536643.html">The Essence of the Iterator Pattern</a> by Saad Alfoudari and
 * L.J. Steggles for further work.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Traversable[T[_]] extends Functor[T] {
  /**
   * Maps each element of the environment onto an action, evaluating from left to right and collecting the results.
   *
   * @param f The function to map on each element in the environment.
   * @param ta The collection of elements to map.
   */
  def traverse[F[_], A, B](f: A => F[B], ta: T[A])(implicit a: Applicative[F]): F[T[B]]

  import Traversable._
  
  /**
   * The <code>Functor</code> that results from a traversable.
   */
  def fmap[A, B](f: A => B, t: T[A]) = traverse[Id, A, B](g => id(f(g)), t).id
}

import FoldRightW._
import validation.Validation

/**
 * Functions over traversable values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Traversable {
  /**
   * A traversable for <code>Id</code>.
   */
  implicit val IdApplicative = new Applicative[Id] {
    def unit[A](a: A) = id(a)
    def apply[A, B](f: Id[A => B], a: Id[A]) = id(f.id(a.id))
  }
  
  /**
   * A traversable for <code>scala.Option</code>.
   */
  implicit val OptionTraversable = new Traversable[Option] {
    def traverse[F[_], A, B](f: A => F[B], ta: Option[A])(implicit a: Applicative[F]): F[Option[B]] =
      ta match {
        case None => a.unit(None)
        case Some(x) => a.fmap(Some(_: B), f(x))
      }
  }

  /**
   * A traversable for <code>scala.List</code>.
   */
  implicit val ListTraversable: Traversable[List] = new Traversable[List] {
    def traverse[F[_], A, B](f: A => F[B], as: List[A])(implicit a: Applicative[F]): F[List[B]] =
      as.foldRight[F[List[B]]](a.unit(Nil))((x, ys) =>
              a(a.fmap((a: B) => (b: List[B]) => a :: b, f(x)), ys))
  }

  /**
   * A traversable for <code>scala.Stream</code>.
   */
  implicit val StreamTraversable: Traversable[Stream] = new Traversable[Stream] {
    def traverse[F[_], A, B](f: A => F[B], as: Stream[A])(implicit a: Applicative[F]): F[Stream[B]] =
      as.foldr[F[Stream[B]]](a.unit(Stream.empty), (x, ys) => a(a.fmap((a: B) => (b: Stream[B]) => Stream.cons(a, b), f(x)), ys))
  }

  /**
   * A traversable for <code>scala.Array</code>.
   */
  implicit val ArrayTraversable: Traversable[Array] = new Traversable[Array] {
    def traverse[F[_], A, B](f: A => F[B], as: Array[A])(implicit a: Applicative[F]): F[Array[B]] =
      a.fmap((_: List[B]).toArray, ListTraversable.traverse[F, A, B](f, as.toList))
  }

  /**
   * A traversable for <code>scala.Either</code>.
   */
  implicit def EitherTraversable[X]: Traversable[PartialType[Either, X]#Apply] = new Traversable[PartialType[Either, X]#Apply] {
    def traverse[F[_], A, B](f: A => F[B], ae: PartialType[Either, X]#Apply[A])(implicit a: Applicative[F]): F[PartialType[Either, X]#Apply[B]] =
      ae match {
        case Left(x) => a.unit(Left(x))
        case Right(x) => a.fmap(Right(_: B), f(x))
      }
  }

  /**
   * A traversable for <code>Validation</code>.
   */
  implicit def ValidationTraversable[X]: Traversable[PartialType[Validation, X]#Apply] = new Traversable[PartialType[Validation, X]#Apply] {
    def traverse[F[_], A, B](f: A => F[B], ae: PartialType[Validation, X]#Apply[A])(implicit a: Applicative[F]): F[PartialType[Validation, X]#Apply[B]] =
      a.fmap((t: Either[X, B]) => (t: Validation[X, B]), EitherTraversable.traverse[F, A, B](f, ae.either))
  }
}
