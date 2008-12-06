// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Wraps <code>Cofunctor</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Cofunctor
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait CofunctorW[F[_], A] {
  /**
   * The value for which there exists an instance of <code>Cofunctor</code>.
   */
  val v: F[A]

  /**
   * The <code>Cofunctor</code> instance.
   */
  val cofunctor: Cofunctor[F]

  /**
   * Maps the given function across this cofunctor.
   */
  def <[B](f: B => A): F[B]

  /**
   * Maps the given function across this cofunctor.
   */
  def ->:[B](f: B => A) = <(f)
}

/**
 * Functions over contra-variant functors.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object CofunctorW {
  /**
   * Constructs a contra-variant functor from the given value and implementation.
   */
  def cofunctor[F[_], A](ft: F[A])(implicit f: Cofunctor[F]): CofunctorW[F, A] = new CofunctorW[F, A] {
    val v = ft
    val cofunctor = f
    def <[B](ff: B => A) = f.comap(ff, ft)
  }

  /**
   * A cofunctor for <code>scala.Function1</code>.
   */
  implicit def Function1Functor[B, C](as: C => B): CofunctorW[PartialType[Function1, B]#Flip, C] = cofunctor[PartialType[Function1, B]#Flip, C](as)
}
