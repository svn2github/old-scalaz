package scalaz.scalacheck

object FoldLeft {
  import org.scalacheck.Constraint

  implicit val ConstraintFoldLeft = new FoldLeft[Constraint] {
    def foldLeft[B, A](t: Constraint[A], b: B, f: (B, A) => B) = f(b, t.unbox)
  }
}
