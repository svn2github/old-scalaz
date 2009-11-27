package scalaz

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

object Apply {
  import Scalaz._

  implicit val IdentityApply: Apply[Identity] = FunctorBindApply

  implicit val Function0Apply: Apply[Function0] = FunctorBindApply

  implicit val ListApply: Apply[List] = FunctorBindApply

  implicit val OptionApply: Apply[Option] = FunctorBindApply
}
