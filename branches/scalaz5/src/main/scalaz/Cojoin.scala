package scalaz

trait Cojoin[M[_]] {
  def cojoin[A](a: M[A]): M[M[A]]
}

object Cojoin {
  import Scalaz._
  
  implicit val IdentityCojoin: Cojoin[Identity] = new Cojoin[Identity] {
    def cojoin[A](a: Identity[A]) = a
  }

  implicit val Function0Cojoin: Cojoin[Function0] = new Cojoin[Function0] {
    def cojoin[A](a: Function0[A]) = () => a
  }
}
