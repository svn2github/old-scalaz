package scalaz.scalacheck

trait Applicative {
  import org.scalacheck.{Gen, Arbitrary}
  import Scalaz._
  import scalaz.Applicative._
  import Applicative._
  import scalaz.Pointed._
  import Pointed._
  import scalaz.Apply._
  import Apply._
  import Pure._

  implicit val GenApplicative = applicative[Gen]

  implicit val ArbitraryApplicative = applicative[Arbitrary]
}
