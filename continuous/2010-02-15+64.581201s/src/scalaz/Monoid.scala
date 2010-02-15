package scalaz

trait Monoid[M] extends Zero[M] with Semigroup[M]

abstract class MonoidLow {
  implicit def monoid[M](implicit s: Semigroup[M], z: Zero[M]): Monoid[M] = new Monoid[M] {
    def append(s1: M, s2: => M) = s append (s1, s2)

    val zero = z.zero
  }
}

object Monoid extends MonoidLow {
  import Semigroup._
  import Zero._

  implicit def EitherLeftMonoid[A, B](implicit bz: Zero[B]): Monoid[Either.LeftProjection[A, B]] = monoid[Either.LeftProjection[A, B]](EitherLeftSemigroup, EitherLeftZero[A, B](bz))
}
