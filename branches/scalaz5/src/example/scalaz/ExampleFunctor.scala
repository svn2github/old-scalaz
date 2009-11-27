package scalaz

object ExampleFunctor {
  import Scalaz._
  
  def main(args: Array[String]) {
    // Functor map
    println(List(1, 2, 3, 4, 5) ∘ (1+))
  }
}
