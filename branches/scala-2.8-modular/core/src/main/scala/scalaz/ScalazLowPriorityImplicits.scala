package scalaz


class ScalazLowPriorityImplicits {
  implicit def ma[M[_], A](a: M[A]): MA[M, A] = MA.ma(a)
  implicit def mma[M[_], A](m: M[M[A]]): MMA[M, A] = MMA.mma(m)
}
