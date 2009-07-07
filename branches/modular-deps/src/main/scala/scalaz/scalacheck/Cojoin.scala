package scalaz.scalacheck

object Cojoin {
  import org.scalacheck.Constraint
  import Constraint.{Pos, Neg, Alpha, Numeric, AlphaNum, Small, Large}

  implicit val ConstraintCojoin: Cojoin[Constraint] = new Cojoin[Constraint] {
    def cojoin[A](a: Constraint[A]) = a match {
      case Pos(t) => Pos(Pos(t))
      case Neg(t) => Neg(Neg(t))
      case Alpha(t) => Alpha(Alpha(t))
      case Numeric(t) => Numeric(Numeric(t))
      case AlphaNum(t) => AlphaNum(AlphaNum(t))
      case Small(t) => Small(Small(t))
      case Large(t) => Large(Large(t))
      case c => new Constraint[Constraint[A]] {
        def unbox = c
      }
    }
  }
}