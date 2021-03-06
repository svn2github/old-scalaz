package scalaz
package http

import java.io.InputStream
import Scalaz._

/**
 * Takes an input-stream to an environment for bytes.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
sealed trait InputStreamer[I[_]] {
  /**
   * Transforms the given input-stream to an environment for bytes.
   */
  def apply(in: InputStream): I[Byte]
}

/**
 * Functions over values that take an input-stream to an environment for bytes.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
object InputStreamer {
  /**
   * Constructs an input-streamer from the given function.
   */
  def inputStreamer[I[_]](f: InputStream => I[Byte]) = new InputStreamer[I] {
    def apply(in: InputStream) = f(in)
  }

  /**
   * An input-streamer for <code>scala.Stream</code> that reads off the input-stream.
   */
  implicit val StreamInputStreamer: InputStreamer[Stream] = inputStreamer[Stream](_.stream)

  /**
   * An input-streamer for <code>scala.Iterator</code> that reads off the input-stream. 
   */
  implicit val IteratorInputStreamer: InputStreamer[Iterator] = inputStreamer[Iterator](_.elements)
}
