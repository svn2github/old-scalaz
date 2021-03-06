package scalaz.http.request

import Scalaz._

/**
 * Kleisli implementations for request parameter retrieval.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
object Param {
  /**
   * A kleisli for requesting the first get parameter from a request.
   */
  def g(implicit r: Request[IN] forSome { type IN[_] }) = kleisli[Option]((p: String) => r ! p)

  /**
   * A kleisli for requesting all get parameters from a request.
   */
  def gg(implicit r: Request[IN] forSome { type IN[_] }) = kleisli[List]((p: String) => r !! p)  
}
