package scalaz

object ExampleFunctor {
  def main(args: Array[String]) = run

  import Scalaz._
  
  def run {
    // Map across the List functor
    List(1, 2, 3, 4, 5) ∘ (1+) println

    // Map across the Option functor
    some(7) ∘ (1+) println
  }
}
