package scalaz.scalacheck

object Copure {
  import org.scalacheck.Constraint

  implicit val ConstraintCopure = new Copure[Constraint] {
    def copure[A](a: Constraint[A]) = a.unbox
  }
}