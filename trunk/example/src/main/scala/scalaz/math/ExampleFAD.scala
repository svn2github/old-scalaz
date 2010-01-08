package scalaz
package math

import scalaz._
import Scalaz._


object ExampleFAD {
  def main(args: Array[String]) = run

  def run {
    import scalaz.math._
    import FAD._

    def f[T](x: T)(implicit r: Real[T]) = {
      import r._
      
      x * 2. + x * x
    }

    f(1.).toDouble assert_≟ 3.0
    diff((x: Dual[Double]) => f(x))(1.).toDouble assert_≟ 4.0
  }
}
