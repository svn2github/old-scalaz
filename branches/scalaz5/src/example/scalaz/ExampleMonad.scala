package scalaz

object ExampleMonad {
  def main(args: Array[String]) = run

  import Scalaz._
  import collection.mutable.GenericArray
  
  def run {
    // zeroOr
    some(7).zeroOr[List].println
    none[Int].zeroOr[List].println
    some(7).zeroOr[GenericArray].println
    some(7).zeroOr[Option].println

    // guard
    true.guard[List](7).println
    false.guard[List](7).println
    true.guard[Option](7).println
    false.guard[Option](7).println

    // prevent
    true.prevent[List](8).println
    false.prevent[List](8).println
    true.prevent[Option](8).println
    false.prevent[Option](8).println

    // join μ
    (some(some(9)) μ).println
    (some(none[Int]) μ).println
    (none[Option[Int]] μ).println
    (List(List(1, 2, 3), List(4, 5, 6)) μ).println
  }
}
