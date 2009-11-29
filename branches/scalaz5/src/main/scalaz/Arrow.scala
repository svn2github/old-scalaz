package scalaz

trait Arrow[A[_, _]] {
  val category: Category[A]
  
  def arrow[B, C](f: B => C): A[B, C]

  def first[B, C, D](a: A[B, C]): A[(B, D), (C, D)]

  def second[B, C, D](a: A[B, C]): A[(D, B), (D, C)]
}

object Arrow {
  import Scalaz._
  
  implicit val Function1Arrow: Arrow[Function1] = new Arrow[Function1] {
    val category = Category.Function1Category
    
    def arrow[B, C](f: B => C) = f

    def first[B, C, D](a: B => C) =
      (bd: (B, D)) => (a(bd._1), bd._2)

    def second[B, C, D](a: B => C) =
      (db: (D, B)) => (db._1, a(db._2))
  }

  implicit def KleisliArrow[M[_]](implicit m: Monad[M]): Arrow[PartialApplyK[Kleisli, M]#Apply] = new Arrow[PartialApplyK[Kleisli, M]#Apply] {
    val category = Category.KleisliCategory

    def arrow[B, C](f: B => C) = ☆(f(_) η)

    def first[B, C, D](a: Kleisli[M, B, C]) = ☆ {
      case (b, d) => a(b) ∘ ((_, d))
    }

    def second[B, C, D](a: Kleisli[M, B, C]) = ☆ {
      case (d, b) => a(b) ∘ ((d, _))
    }
  }
}
