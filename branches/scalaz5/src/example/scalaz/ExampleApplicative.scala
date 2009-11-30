package scalaz

object ExampleApplicative {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Applicative functor apply
    List(40, 50, 60) ⊛ (List(1, 2, 3) ∘ ((_: Int) * (_: Int)).curry) println
  
    // Applicative functor lift
    List(1, 2, 3) <⊛> (List(40, 50, 60), (_: Int) * (_: Int)) println
  
    // Applicative functor lift to pair
    List(1, 2, 3) <×> List(40, 50, 60) println

    // Applicative functor lift (anonymous right)
    List(1, 2, 3) ⊛> List(40, 50, 60) println

    // Applicative functor lift (anonymous left)
    List(1, 2, 3) <⊛ List(40, 50, 60) println
  }
}