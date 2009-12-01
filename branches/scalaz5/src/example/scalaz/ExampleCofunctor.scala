package scalaz

object ExampleCofunctor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Contravariant functor map
    {
      val f = (3 + (_: Int))
      List(1, 2, 3, 4, 5) ∘ (f ∙ ((_: Int) / 2)) println
    }
  }
}