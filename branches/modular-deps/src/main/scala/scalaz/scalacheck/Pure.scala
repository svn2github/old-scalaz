package scalaz.scalacheck


object Pure {
  import org.scalacheck.{Gen, Arbitrary}

  implicit val GenPure: Pure[Gen] = new Pure[Gen] {
    def pure[A](a: => A) = {
      def fun() = a
      Gen.value(fun)
    }
  }

  implicit val ArbitraryPure: Pure[Arbitrary] = new Pure[Arbitrary] {
    def pure[A](a: => A) = {
      def fun() = a
      Arbitrary(Gen.value(fun))
    }
  }
}


