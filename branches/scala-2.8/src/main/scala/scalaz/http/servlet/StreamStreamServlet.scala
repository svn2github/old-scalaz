package scalaz
package http
package servlet

import HttpServletRequest._
import HttpServletResponse._

final class StreamStreamServlet extends
  ScalazServlet[Stream, Stream, StreamStreamServletApplication](classOf[StreamStreamServletApplication])
