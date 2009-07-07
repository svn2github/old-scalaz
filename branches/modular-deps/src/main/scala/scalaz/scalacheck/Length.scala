package scalaz.scalacheck

object Length {
  import org.scalacheck.Constraint

  implicit val ConstraintLength = new Length[Constraint] {
    def len[A](a: Constraint[A]) = 1
  }
}