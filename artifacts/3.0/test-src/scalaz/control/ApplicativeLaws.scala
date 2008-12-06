package scalaz.control

import reductios.Property._
import reductios.Arbitrary
import EqualW._

object ApplicativeLaws {
  def identity[A[_], X](implicit a: Applicative[A],
                                 aax: Arbitrary[A[X]],
                                 e: Equal[A[X]]) = 
    prop((apx: A[X]) => apx === a(a.unit((x: X) => x), apx))

  def composition[A[_], X, Y, Z](implicit a: Applicative[A],
                                aayz: Arbitrary[A[Y => Z]],
                                aaxy: Arbitrary[A[X => Y]],
                                aax: Arbitrary[A[X]],
                                e: Equal[A[Z]]) =
    prop((apyz: A[Y => Z], apxy: A[X => Y], apx: A[X]) =>
            a(apyz, a(apxy, apx)) ===
            a(a(a(a.unit((f: Y => Z) => (g: X => Y) => f compose g), apyz), apxy), apx))

  def homomorphism[A[_], X, Y](implicit a: Applicative[A],
                                        ax: Arbitrary[X],
                                        axy: Arbitrary[X => Y],
                                        e: Equal[A[Y]]) =
    prop((f: X => Y, x: X) => a(a.unit(f), a.unit(x)) === a.unit(f(x)))

  def interchange[A[_], X, Y](implicit a: Applicative[A],
                                       ax: Arbitrary[X],
                                       apxy: Arbitrary[A[X => Y]],
                                       e: Equal[A[Y]]) =
    prop((f: A[X => Y], x: X) =>
      a(f, a.unit(x)) === a(a.unit((f: X => Y) => f(x)), f))
}
