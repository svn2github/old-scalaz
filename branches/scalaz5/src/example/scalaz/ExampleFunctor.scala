package scalaz

object ExampleFunctor {
  def main(args: Array[String]) = run

  import Scalaz._
  
  def run {
    // Map across the List functor
    println(List(1, 2, 3, 4, 5) ∘ (1+))

    // Map across the Option functor
    println(some(7) ∘ (1+))
  }
}
