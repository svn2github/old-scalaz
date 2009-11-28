package scalaz

object Scalaz extends ScalazLow
              with    Kleislis
              with    Identitys
              with    Digits
              with    Alphas
              with    DLists
              with    Booleans
              with    Endos
              with    Enumerations
              with    Function0s
              with    Function1s
              with    Function2s
              with    InputStreams
              with    Longs
              with    States
              with    Memos
              with    Chars {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }

  def ⊥ = error("undefined")

  type ⊤ = Any

  type ℤ = BigInt
}
