package scalaz.scalacheck

object Index {
  import org.scalacheck.Constraint

  implicit val ConstraintIndex = new Index[Constraint] {
    def index[A](a: Constraint[A], i: Int) = if(i == 0) Some(a.unbox) else None
  }
}
