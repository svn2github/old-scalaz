package scalaz.scalacheck

object FoldRight {
  import org.scalacheck.Constraint

  implicit val ConstraintFoldRight = new FoldRight[Constraint] {
    def foldRight[A, B](t: Constraint[A], b: B, f: (A, => B) => B) = f(t.unbox, b)
  }
}
