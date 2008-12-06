// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Function application within an environment.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Apply[AP[_]] {
  /**
   * Apply the given function in the given value through the environment.
   */
  def apply[A, B](f: AP[A => B], a: AP[A]): AP[B]  
}
