// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * A monad with empty and plus.
 *
 * <p>
 * All monad-empty-plus instances must satisfy the monad laws, the monad-empty laws and the monad-plus law.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MonadEmptyPlus[M[_]] extends MonadPlus[M] with MonadEmpty[M]

/**
 * Functions over monad-empty-plus values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadEmptyPlus {
  import MonadEmpty._
  import Monad._
  import Functor._

  /**
   * Lifts the given value into unit using the given monad-empty-plus.
   */
  def unit[A, U[_]](a: A)(implicit u: MonadEmptyPlus[U]) = u.unit(a)

  /**
   * A monad-empty-plus for <code>scala.Option</code>.
   */
  implicit val OptionMonadEmptyPlus = new MonadEmptyPlus[Option] {
    def plus[A](a1: Option[A], a2: Option[A]) = if(a1.isEmpty) a2 else a1
    def empty[A] = None // WTF!?!
    def unit[A](a: A) = Some(a) // WTF? Calling OptionMonad.unit(a) crashes the type checker
    def bind[A, B](f: A => Option[B], o: Option[A]) = OptionMonad.bind(f, o)
  }

  /**
   * A monad-empty-plus for <code>scala.List</code>.
   */
  implicit val ListMonadEmptyPlus = new MonadEmptyPlus[List] {
    def plus[A](a1: List[A], a2: List[A]) = a1 ::: a2
    def empty[A] = Nil
    def unit[A](a: A) = a :: Nil
    def bind[A, B](f: A => List[B], o: List[A]) = ListMonad.bind(f, o)
  }

  /**
   * A monad-empty-plus for <code>scala.Stream</code>.
   */
  implicit val StreamMonadEmptyPlus = new MonadEmptyPlus[Stream] {
    def plus[A](a1: Stream[A], a2: Stream[A]) = a1 append a2
    def empty[A] = Stream.empty
    def unit[A](a: A) = StreamMonad.unit(a)
    def bind[A, B](f: A => Stream[B], o: Stream[A]) = StreamMonad.bind(f, o)
  }

  /**
   * A monad-empty-plus for <code>scala.Array</code>.
   */
  implicit val ArrayMonadEmptyPlus = new MonadEmptyPlus[Array] {
    def plus[A](a1: Array[A], a2: Array[A]) = a1 ++ a2
    def empty[A] = new Array(0)
    def unit[A](a: A) = ArrayMonad.unit(a)
    def bind[A, B](f: A => Array[B], o: Array[A]) = ArrayMonad.bind(f, o)
  }

  /**
   * Constructs a container using the given function and begin value.
   *
   * @param f The function to unfold to construct the container.
   * @param b The beginning value to start unfolding with.
   */
  def unfold[A, B, M[_]](f: B => Option[(A, B)], b: B)(implicit m: MonadEmptyPlus[M]): M[A] = f(b) match {
    case None => m.empty
    case Some((a, b)) => m.plus(m.unit(a), unfold[A, B, M](f, b))
  }

  /**
   * Replicates the given value the given number of times.
   *
   * @param n The number of times to replicate the given value.
   * @param a The value to replicate.
   */
  def replicate[A, M[_]](n: Int, a: A)(implicit m: MonadEmptyPlus[M]): M[A] =
    if(n <= 0) m.empty
    else m.plus(m.unit(a), replicate[A, M](n - 1, a))

  /**
   * Sums the given iterable by sequencing.
   */
  def sum[A, F[_], M[_]](as: F[M[A]])(implicit f: FoldRight[F], m: MonadEmptyPlus[M]) =
    f.foldRight[M[A], M[A]](as, m.empty, m.plus(_, _))
}
