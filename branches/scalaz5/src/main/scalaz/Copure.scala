package scalaz

trait Copure[-C[_]] {
  def copure[A](p: C[A]): A
}

object Copure {
  import Scalaz._
  
  implicit val IdentityCopure: Copure[Identity] = new Copure[Identity] {
    def copure[A](a: Identity[A]) = a
  }

  implicit val Function0Copure: Copure[Function0] = new Copure[Function0] {
    def copure[A](a: Function0[A]) = a.apply
  }
}