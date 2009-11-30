package scalaz

object ExampleApplicative {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Applicative functor apply
    println(List(40, 50, 60) ⊛ (List(1, 2, 3) ∘ ((_: Int) * (_: Int)).curry))
  
    // Applicative functor lift
    println(List(1, 2, 3) <⊛> (List(40, 50, 60), (_: Int) * (_: Int)))
  
    // Applicative functor lift to pair
    println(List(1, 2, 3) <×> List(40, 50, 60))

    // Applicative functor lift (anonymous right)
    println(List(1, 2, 3) ⊛> List(40, 50, 60))

    // Applicative functor lift (anonymous left)
    println(List(1, 2, 3) <⊛ List(40, 50, 60))
  }
}