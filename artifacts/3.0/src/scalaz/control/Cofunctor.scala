// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Contra-variant function application in an environment.
 *
 * <p>
 * All contra-variant functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == comap(identity, a)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g. comap(f compose g, a) == comap(g, comap(f, a))</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Cofunctor[F[_]] {
  /**
   * Map the given function across the given contra-variant functor.
   */
  def comap[A, B](f: B => A, fa: F[A]): F[B]  
}

/**
 * Functions over contra-variant functors.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Cofunctor {
  /**
   * A contra-variant functor for <code>scala.Function1</code>.
   */
  implicit def Function1Cofunctor[A]: Cofunctor[PartialType[Function1, A]#Flip] = new Cofunctor[PartialType[Function1, A]#Flip] {
    def comap[B, C](f: C => B, oa: PartialType[Function1, A]#Flip[B]) = oa compose f 
  }
}
