package scalaz

import org.scalacheck.{Gen, Arbitrary}
import scalaz.MMA._

trait MMAImplicits {
  implicit def GenMMA[A](a: Gen[Gen[A]]) = mmaPartial[Gen](a)

  implicit def ArbitraryMMA[A](a: Arbitrary[Arbitrary[A]]) = mmaPartial[Arbitrary](a)

}
