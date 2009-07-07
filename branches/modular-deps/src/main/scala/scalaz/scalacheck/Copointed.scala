package scalaz.scalacheck

object Copointed {
  import org.scalacheck.Constraint
  import scalaz.Copointed._
  import scalaz.Functor._
  import Functor._
  import scalaz.Copure._
  import Copure._

  implicit val ConstraintCopointed = copointed[Constraint]
}
