package scalaz

// compiles == green
object ImplicitConversionTest {
  import scalaz.Scalaz._

  def MAs {
    implicitly[List[Int] <%%< MA[List, Int]]
    implicitly[Option[Int] <%%< MA[Option, Int]]

    implicitly[(Int => String) <%%< MACofunctor[PartialApply1Of2[Function1, String]#Flip, Int]]
    implicitly[(Int => String) <%%< MA[PartialApply1Of2[Function1, Int]#Apply, String]]

    // via higher kind inference
    trait T[A]
    implicitly[T[Int] <%%< MACofunctor[T, Int]]
    implicitly[T[Int] <%%< MA[T, Int]]
  }

  def apply {
    implicitly[Apply[Identity]]
    implicitly[Apply[List]]
    implicitly[Apply[Function0]]
    implicitly[Apply[Option]]
  }

  def monad {
    implicitly[Monad[List]]
    implicitly[Monad[Stream]]
  }


  def partialApply {
    trait A
    trait B
    trait C
    trait D
    trait E
    trait F
    trait G

    trait T1[A]
    trait T2[A, B]
    trait T3[A, B, C]
    trait T4[A, B, C, D]
    trait T5[A, B, C, D, E]
    trait T6[A, B, C, D, E, F]
    trait T7[A, B, C, D, E, F, G]

    implicitly[PartialApply1Of2[T2, A]#Apply[B] =::= T2[A, B]]
    implicitly[PartialApply1Of2[T2, A]#Flip[B] =::= T2[B, A]]

    implicitly[PartialApply2Of3[T3, A, B]#Apply[C] =::= T3[A, B, C]]
    implicitly[PartialApply2Of3[T3, A, B]#ApplyA[C] =::= T3[C, A, B]]
    implicitly[PartialApply2Of3[T3, A, B]#ApplyB[C] =::= T3[A, C, B]]

    implicitly[PartialApply3Of4[T4, A, B, C]#Apply[D] =::= T4[A, B, C, D]]

    implicitly[PartialApply4Of5[T5, A, B, C, D]#Apply[E] =::= T5[A, B, C, D, E]]

    implicitly[PartialApply5Of6[T6, A, B, C, D, E]#Apply[F] =::= T6[A, B, C, D, E, F]]

    implicitly[PartialApply6Of7[T7, A, B, C, D, E, F]#Apply[G] =::= T7[A, B, C, D, E, F, G]]
  }
}