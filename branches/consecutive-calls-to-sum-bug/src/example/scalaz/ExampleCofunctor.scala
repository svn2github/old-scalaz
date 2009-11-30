package scalaz

object ExampleCofunctor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Contravariant functor map
    {
      val f = ma[PartialApply1Of2[Function1, Int]#Flip, Int](3+) // todo This is icky. Posted to mailing list.
      List(1, 2, 3, 4, 5) ∘ (f ∙ ((_: Int) / 2)) println
    }
  }
}