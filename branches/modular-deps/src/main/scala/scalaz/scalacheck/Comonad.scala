package scalaz.scalacheck

object Comonad {
  import org.scalacheck.Constraint
  import scalaz.Comonad._
  import scalaz.Cojoin._
  import Cojoin._
  import Comonad._
  import scalaz.Copointed._
  import Copointed._
  import Scalaz._
  import ScalazScalaCheck._

  implicit val ConstraintComonad = comonad[Constraint]
}
