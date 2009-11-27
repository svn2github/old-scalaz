package scalaz

object ExampleFunctor {
  import Scalaz._
  
  def run {
    // Functor map
    println(List(1, 2, 3, 4, 5) ∘ (1+))
  }

  def main(args: Array[String]) = run
}
