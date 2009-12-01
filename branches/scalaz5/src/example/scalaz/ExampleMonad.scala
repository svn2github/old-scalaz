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
  }
}
