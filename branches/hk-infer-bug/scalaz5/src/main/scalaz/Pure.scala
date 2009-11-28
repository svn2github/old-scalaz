package scalaz

trait Pure[+P[_]] {
  def pure[A](a: => A): P[A]
}

object Pure {
  import Scalaz._

  implicit val IdentityPure: Pure[Identity] = new Pure[Identity] {
    def pure[A](a: => A) = a
  }

  implicit val Function0Pure = new Pure[Function0] {
    def pure[A](a: => A) = new Function0[A] {
      def apply = a
    }
  }

  implicit val ListPure = new Pure[List] {
    def pure[A](a: => A) = List(a)
  }

  implicit val OptionPure = new Pure[Option] {
    def pure[A](a: => A) = Some(a)
  }
}
