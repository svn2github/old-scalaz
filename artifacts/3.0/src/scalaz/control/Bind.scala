// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Binds a function through an environment (known also as sequencing).
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Bind[B[_]] {
  /**
   * Binds the given value with the given value through the environment.
   */
  def bind[A, X](f: A => B[X], ba: B[A]): B[X]
}
