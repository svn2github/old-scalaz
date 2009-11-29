package scalaz

trait Category[C[_, _]] {
  def id[A]: C[A, A]
  def compose[X, Y, Z](f: C[Y, Z], g: C[X, Y]): C[X, Z]
}

object Category {
  import Scalaz._
  
  implicit val Function1Category: Category[Function1] = new Category[Function1] {
    def id[A] = a => a
    def compose[X, Y, Z](f: Y => Z, g: X => Y) = f compose g   
  }

  implicit def KleisliCategory[M[_]](implicit m: Monad[M]): Category[PartialApplyK[Kleisli, M]#Apply] = new Category[PartialApplyK[Kleisli, M]#Apply] {
    def id[A] = ☆(_ η)
    def compose[X, Y, Z](f: Kleisli[M, Y, Z], g: Kleisli[M, X, Y]) = f <=< g 
  }
}
