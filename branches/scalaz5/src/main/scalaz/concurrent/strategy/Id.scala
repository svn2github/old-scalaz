package scalaz.concurrent.strategy

import scalaz.concurrent.Strategy

/**
 * A strategy that performs no evaluation of its argument.
 */
trait Id {
  implicit def strategy[A]: Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = a
  }
}
