// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * A monad that also has an empty value.
 *
 * <p>
 * All monad-empty instances must satisfy the monad laws and 2 additional laws:
 * <ol>
 * <li><strong>left identity (empty)</strong><br/><code>forall f. empty == bind(f, empty)</code></li>
 * <li><strong>right identity (empty)</strong><br/><code>forall a. empty == bind(x => a, empty)</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MonadEmpty[M[_]] extends Monad[M] with Empty[M]

/**
 * Functions over monad-empty values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadEmpty {
  import Monad._
  import Functor._

  /**
   * Lifts the given value into unit using the given monad-empty.
   */
  def unit[A, U[_]](a: A)(implicit u: Applicative[U]) = u.unit(a)

  /**
   * A monad-empty for <code>scala.Option</code>.
   */
  implicit val OptionMonadEmpty = new MonadEmpty[Option] {
    def empty[A] = None
    def unit[A](a: A) = Some(a) // WTF? Calling OptionMonad.unit(a) crashes the type checker
    def bind[A, B](f: A => Option[B], o: Option[A]) = OptionMonad.bind(f, o)
  }

  /**
   * A monad-empty for <code>scala.List</code>.
   */
  implicit val ListMonadEmpty = new MonadEmpty[List] {
    def empty[A] = Nil
    def unit[A](a: A) = a :: Nil
    def bind[A, B](f: A => List[B], o: List[A]) = ListMonad.bind(f, o)
  }

  /**
   * A monad-empty for <code>scala.Stream</code>.
   */
  implicit val StreamMonadEmpty = new MonadEmpty[Stream] {
    def empty[A] = Stream.empty
    def unit[A](a: A) = StreamMonad.unit(a)
    def bind[A, B](f: A => Stream[B], o: Stream[A]) = StreamMonad.bind(f, o)
  }

  /**
   * A monad-empty for <code>scala.Array</code>.
   */
  implicit val ArrayMonadEmpty = new MonadEmpty[Array] {
    def empty[A] = new Array(0)
    def unit[A](a: A) = ArrayMonad.unit(a)
    def bind[A, B](f: A => Array[B], o: Array[A]) = ArrayMonad.bind(f, o)
  }

  /**
   * If the given condition is <code>true</code> then return the monadic unit value, otherwise, return the given empty,
   * which may contain a side-effect.
   */
  def guard[M[_]](c: Boolean)(implicit m: MonadEmpty[M]): M[scala.Unit] =
    if(c) m.unit(()) else m.empty

  /**
   * If the given condition is <code>false</code> then return the monadic unit value, otherwise, return the given empty,
   * which may contain a side-effect.
   */
  def prevent[M[_]](c: Boolean)(implicit m: MonadEmpty[M]): M[scala.Unit] =
    if(c) m.empty else m.unit(())
}
