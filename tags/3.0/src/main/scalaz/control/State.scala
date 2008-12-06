// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * Stateful computation.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait State[S, A] {
  /**
   * Perform the computation and pass the state through.
   */
  def state(s: S): (S, A)
}

/**
 * Functions over state.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object State {
  /**
   * Constructs a state from the given state-passing function.
   */
  def state[S, A](f: S => (S, A)) = new State[S, A] {
    def state(s: S) = f(s)
  }
}
