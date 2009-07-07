package scalaz.scalacheck

object Apply {
  import org.scalacheck.{Gen, Arbitrary}
  import scalaz.Apply._
  import Scalaz._
  import ScalazScalaCheck._

  import Bind._
  import Functor._

  implicit val GenApply = FunctorBindApply[Gen]

  implicit val ArbitraryApply = FunctorBindApply[Arbitrary]
}

