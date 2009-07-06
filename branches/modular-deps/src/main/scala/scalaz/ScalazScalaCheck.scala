package scalaz

object ScalazScalaCheck extends ScalazScalaCheck

trait ScalazScalaCheck {

  import MA.ma

  import org.scalacheck.{Gen, Arbitrary, Constraint}

  implicit def GenMA[A](a: Gen[A]) = ma[Gen](a)

  implicit def ArbitraryMA[A](a: Arbitrary[A]) = ma[Arbitrary](a)

  implicit def ConstraintMA[A](a: Constraint[A]) = ma[Constraint](a)

  // MMA

  import MMA.mma

  import org.scalacheck.{Gen, Arbitrary, Constraint}

  implicit def GenMMA[A](a: Gen[Gen[A]]) = mma[Gen](a)

  implicit def ArbitraryMMA[A](a: Arbitrary[Arbitrary[A]]) = mma[Arbitrary](a)

  implicit def ConstraintMMA[A](a: Constraint[Constraint[A]]) = mma[Constraint](a) 
}