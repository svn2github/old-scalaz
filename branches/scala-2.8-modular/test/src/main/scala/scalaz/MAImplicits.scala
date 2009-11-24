package scalaz

import org.scalacheck.{Gen, Arbitrary}
import scalaz.MA._

trait MAImplicits {
  implicit def GenMA[A](a: Gen[A]) = maPartial[Gen](a)

  implicit def ArbitraryMA[A](a: Arbitrary[A]) = maPartial[Arbitrary](a)
}
