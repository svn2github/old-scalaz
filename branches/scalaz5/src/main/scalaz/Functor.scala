package scalaz

trait Functor[F[_]] {
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

object Functor {
  import Scalaz._

  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](r: Identity[A], f: A => B) = f(r.value)
  }

  implicit val Function0Functor = new Functor[Function0] {
    def fmap[A, B](r: Function0[A], f: A => B) = new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit def Function1Functor[R] = new Functor[PartialApply1Of2[Function1, R]#Apply] {
    def fmap[A, B](r: R => A, f: A => B) = r andThen f
  }
        
  implicit val ListFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](r: List[A], f: A => B) = r map f
  }

  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](r: Option[A], f: A => B) = r map f
  }
}