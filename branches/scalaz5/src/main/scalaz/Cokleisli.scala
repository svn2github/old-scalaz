package scalaz

trait Cokleisli[W[_], A, B] {
  def apply(a: W[A]): B

  import Scalaz._

  def <<=(a: W[A])(implicit w: Comonad[W]) = a ⇤ apply
}

object Cokleisli {
  def cokleisli[W[_], A, B](f: W[A] => B) = new Cokleisli[W, A, B] {
    def apply(a: W[A]) = f(a)
  }
}
