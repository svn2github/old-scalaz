package scalaz.scalacheck

object Pointed {
  import org.scalacheck.{Gen, Arbitrary}
  import scalaz.Pointed._
  import Pointed._
  import scalaz.Functor._
  import Functor._
  import scalaz.Pure._
  import Pure._
  import Scalaz._
  import ScalazScalaCheck._

  implicit val GenPointed = pointed[Gen]

  implicit val ArbitraryPointed = pointed[Arbitrary]
}
