package scalaz.concurrent

trait Strategy[A] {
  def apply(a: () => A): () => A
}

trait Strategys {
  implicit def strategyFrom[A](f: (() => A) => () => A): Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = f(a)
  }

  implicit def strategyTo[A](s: Strategy[A]): (() => A) => () => A = (a: () => A) => s(a)
}
