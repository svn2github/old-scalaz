package scalaz.scalacheck

object Bind {
  import org.scalacheck.{Gen, Arbitrary}

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](r: Gen[A], f: A => Gen[B]) = r flatMap f
  }

  implicit val ArbitraryBind: Bind[Arbitrary] = new Bind[Arbitrary] {
    def bind[A, B](r: Arbitrary[A], f: A => Arbitrary[B]) = Arbitrary(r.arbitrary flatMap (f(_).arbitrary))
  }
}
