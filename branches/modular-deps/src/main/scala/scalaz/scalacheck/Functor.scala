package scalaz.scalacheck

object Functor {
  import org.scalacheck.{Gen, Arbitrary, Constraint}
  import Constraint.{Pos, Neg, Alpha, Numeric, AlphaNum, Small, Large}
  import scalaz.Functor._
  import Scalaz._
  import ScalazScalaCheck._

  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](r: Gen[A], f: A => B) = r map f
  }

  implicit val ArbitraryFunctor: Functor[Arbitrary] = new Functor[Arbitrary] {
    def fmap[A, B](r: Arbitrary[A], f: A => B) = r map f
  }

  implicit val ConstraintFunctor: Functor[Constraint] = new Functor[Constraint] {
    def fmap[A, B](r: Constraint[A], f: A => B) = r match {
      case Pos(t) => Pos(f(t))
      case Neg(t) => Neg(f(t))
      case Alpha(t) => Alpha(f(t))
      case Numeric(t) => Numeric(f(t))
      case AlphaNum(t) => AlphaNum(f(t))
      case Small(t) => Small(f(t))
      case Large(t) => Large(f(t))
      case c => new Constraint[B] {
        def unbox = f(c.unbox)
      }
    }
  }
}
