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
              with    Chars {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }

  //implicit def Function1MA[R, A](a: R => A): MA[PartialApply1Of2[Function1, R]#Apply, A] = ma[PartialApply1Of2[Function1, R]#Apply, A](a)
}
