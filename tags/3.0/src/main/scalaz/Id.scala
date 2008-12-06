package scalaz

/**
 * Lifted identity.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Id[+A] {
  /**
   * The identity value.
   */
  val id: A
}

/**
 * Functions over identity values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Id {
  /**
   * Constructs an <code>Id</code> from the given function.
   */
  def id[A](a: A) = new Id[A] {
    val id = a 
  }
}
